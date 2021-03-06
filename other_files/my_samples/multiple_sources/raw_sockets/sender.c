#include <string.h>
#include <stdio.h>
#include <assert.h>
#include <errno.h>
#include <unistd.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <sys/ioctl.h>
// #include <linux/if.h>
#include <net/if.h>
#include <linux/if_packet.h>
#include <netinet/ether.h>

int main(void)
{
    // ETH_P_IP = 0x0800 = IPv4, IPPROTO_RAW = 255
    unsigned short ether_type = ETH_P_ALL /* 0x0003 */;
    static const char iface_name[] = "eno2";
    static const unsigned char dest_mac[ETH_ALEN /* 6 */] =
                                           {0x00, 0x00, 0x00, 0x00, 0x00, 0x00};

    errno = 0;
    int sockfd = socket(AF_PACKET, SOCK_RAW, htons(ether_type)); // see raw(7)
    if (sockfd == -1) {
        perror("socket");
        return 1;
    }

    struct ifreq ifr; // See netdevice (7).
    memset(&ifr, 0, sizeof(ifr));
    assert(sizeof(ifr.ifr_name) >= sizeof(iface_name));
    memcpy(ifr.ifr_name, iface_name, sizeof(iface_name));
    // Get ifr.ifr_ifindex from ifr.ifr_name. See netdevice (7):
    errno = 0;
    int ret = ioctl(sockfd, SIOCGIFINDEX, &ifr);
    if (ret < 0) {
        perror("ioctl(SIOCGIFINDEX)");
        close(sockfd);
        return 2;
    }

    struct sockaddr_ll sock_addr; // See packet (7).
    memset(&sock_addr, 0, sizeof(sock_addr));
    sock_addr.sll_family = AF_PACKET; // = PF_PACKET
    sock_addr.sll_protocol = htons(ether_type);
    sock_addr.sll_ifindex = ifr.ifr_ifindex;
    sock_addr.sll_halen = ETH_ALEN;
    memcpy(sock_addr.sll_addr, dest_mac, ETH_ALEN);

    memset(&ifr, 0, sizeof(ifr));
    memcpy(ifr.ifr_name, iface_name, sizeof(iface_name));
    errno = 0;
    ret = ioctl(sockfd, SIOCGIFHWADDR, &ifr);
    if (ret < 0) {
        perror("ioctl(SIOCGIFHWADDR)");
        close(sockfd);
        return 3;
    }

    // max(L2_PAYLOAD) = MTU
/* #define L2_PAYLOAD 46 */
#define L2_PAYLOAD 1500
/* #define L2_PAYLOAD 9000 */
#define BUF_SIZE (sizeof(struct ether_header) + L2_PAYLOAD)
    unsigned char send_buf[BUF_SIZE];
    memset(send_buf, 0, sizeof(send_buf));
    struct ether_header *eh = (struct ether_header *)send_buf;

    memcpy(eh->ether_dhost, dest_mac, ETH_ALEN);
    memcpy(eh->ether_shost, &ifr.ifr_hwaddr.sa_data, ETH_ALEN);
    eh->ether_type = 0xFFFF; // htons(ETH_P_IP)

    size_t tx_len = sizeof(*eh);
    for (int i = 1; i <= L2_PAYLOAD; i++) {
        send_buf[tx_len++] = (unsigned char)i;
    }
    memset(send_buf + sizeof(*eh), 0, 16);

    // backup data:
    unsigned char send_buf_backup[BUF_SIZE];
    memcpy(send_buf_backup, send_buf, tx_len);
    struct sockaddr_ll sock_addr_backup;
    memcpy(&sock_addr_backup, &sock_addr, sizeof(sock_addr));

    size_t packet_size = ((7 + 1) + (6 + 6 + 2) + L2_PAYLOAD + (4 + 8)) * 8;
    size_t iter = 0;
    while (1) {
        ++iter;
        memcpy(send_buf + sizeof(*eh), &iter, sizeof(iter));

        int flags = 0;
        errno = 0;
        ssize_t num_bytes = sendto(sockfd, send_buf, tx_len, flags,
                               (struct sockaddr*)&sock_addr, sizeof(sock_addr));
        if (num_bytes < 0) {
            perror("sendto");
            close(sockfd);
            return 4;
        }

        if ((size_t)num_bytes != tx_len) {
            printf("attempted to send: %zu, actually sent: %zd\n", tx_len,
                    num_bytes);
        }
        /* if (memcmp(send_buf, send_buf_backup, tx_len) != 0) */
        /*     puts("send_buf has been changed..."); */
        /* if (memcmp(&sock_addr, &sock_addr_backup, sizeof(sock_addr)) != 0) */
        /*     puts("sock_addr has been changed..."); */

        if ((iter % 1000) == 0) {
            /* printf("Iteration #%zu\n", iter); */
            printf("Bits sent: %zu\n", iter * packet_size);
        }
    }

    /* close(sockfd); */
    /* return 0; */
}
