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
    static const char iface_name[] = "eno1";
    static const unsigned char dest_mac[ETH_ALEN /* 6 */] =
                                       {0xb8, 0x27, 0xeb, 0xb8, 0x40, 0x22};

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

    /* static const unsigned char flags = 0x02; // 2 steps */
    static const unsigned char flags = 0x00; // 1 step
    static const unsigned char l2_payload[] = {
        0x00, 0x02, 0x00, 0x2c, 0x00, 0x00, flags, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xd4, 0xbe, 0xd9,
        0xff, 0xfe, 0x18, 0x75, 0x81, 0x00, 0x01, 0x00, 0x14, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    };

#define BUF_SIZE (sizeof(struct ether_header) + sizeof(l2_payload))
    unsigned char send_buf[BUF_SIZE];
    struct ether_header *eh = (struct ether_header *)send_buf;

    memcpy(eh->ether_dhost, dest_mac, ETH_ALEN);
    memcpy(eh->ether_shost, &ifr.ifr_hwaddr.sa_data, ETH_ALEN);
    eh->ether_type = htons(ETH_P_1588); // htons(0x88f7); // htons(ETH_P_IP)
    memcpy(send_buf + sizeof(*eh), l2_payload, sizeof(l2_payload));

    size_t tx_len = sizeof(send_buf);
    size_t iter = 0;
    while (1) {
        ++iter;

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

        break;
    }

    close(sockfd);
    return 0;
}
