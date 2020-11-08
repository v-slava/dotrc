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

    errno = 0;
    int sockfd = socket(AF_PACKET, SOCK_RAW, htons(ether_type)); // see raw(7)
    if (sockfd == -1) {
        perror("socket");
        return 1;
    }

    struct ifreq ifr; // See netdevice (7).
    memset(&ifr, 0, sizeof(ifr));
    memcpy(ifr.ifr_name, iface_name, sizeof(iface_name));
    errno = 0;
    int ret = ioctl(sockfd, SIOCGIFINDEX, &ifr);
    if (ret < 0) {
        perror("ioctl(SIOCGIFINDEX)");
        close(sockfd);
        return 2;
    }

    struct sockaddr_ll sock_addr;
    memset(&sock_addr, 0, sizeof(sock_addr));
    sock_addr.sll_family = PF_PACKET;
    sock_addr.sll_ifindex = ifr.ifr_ifindex;
    sock_addr.sll_protocol = htons(ether_type);
    errno = 0;
    ret = bind(sockfd, (struct sockaddr *)&sock_addr, sizeof(sock_addr));
    if (ret < 0)
    {
        perror("bind");
        close(sockfd);
        return 3;
    }

    // Set the network card in promiscuos mode: instructs network card to
    // forward to CPU all frames, not only frames with
    // destination MAC == our MAC.
    memset(&ifr, 0, sizeof(ifr));
    memcpy(ifr.ifr_name, iface_name, sizeof(iface_name));
    errno = 0;
    ret = ioctl(sockfd, SIOCGIFFLAGS, &ifr);
    if (ret < 0) {
        perror("ioctl(SIOCGIFFLAGS)");
        close(sockfd);
        return 4;
    }

    ifr.ifr_flags |= IFF_PROMISC;

    errno = 0;
    ret = ioctl(sockfd, SIOCSIFFLAGS, &ifr);
    if (ret < 0) {
        perror("ioctl(SIOCSIFFLAGS)");
        close(sockfd);
        return 5;
    }

    puts("Receiving...");

    // max(L2_PAYLOAD) = MTU
/* #define L2_PAYLOAD 46 */
#define L2_PAYLOAD 1500
/* #define L2_PAYLOAD 9000 */
#define BUF_SIZE (sizeof(struct ether_header) + L2_PAYLOAD)
    unsigned char recv_buf[BUF_SIZE];

    size_t packet_size = ((7 + 1) + (6 + 6 + 2) + L2_PAYLOAD + (4 + 8)) * 8;
    size_t iter_receiver = 0, total_bits_lost = 0, total_bits_received = 0;
    while (1) {
        int flags = 0;
        errno = 0;
        /* struct sockaddr_ll sock_addr; // See packet (7). */
        memset(&sock_addr, 0, sizeof(sock_addr));
        socklen_t addrlen = sizeof(sock_addr);
        ssize_t num_bytes = recvfrom(sockfd, recv_buf, sizeof(recv_buf), flags,
                               (struct sockaddr*)&sock_addr, &addrlen);
        if (num_bytes < 0) {
            perror("recvfrom");
            close(sockfd);
            return 6;
        }
        assert(addrlen == sizeof(sock_addr));

        /*
        printf("family: %hd, protocol: %04hX, ifindex: %d, hatype: %hu,\n\
pkttype: %hhu, halen: %hhu, addr: %02hhX:%02hhX:%02hhX:%02hhX:%02hhX:%02hhX\n",
sock_addr.sll_family, sock_addr.sll_protocol, sock_addr.sll_ifindex,
sock_addr.sll_hatype, sock_addr.sll_pkttype, sock_addr.sll_halen,
sock_addr.sll_addr[0], sock_addr.sll_addr[1], sock_addr.sll_addr[2],
sock_addr.sll_addr[3], sock_addr.sll_addr[4], sock_addr.sll_addr[5]);
        */

        size_t expected_num_bytes = BUF_SIZE;
        if ((size_t)num_bytes != expected_num_bytes) {
            /* printf("Expected: %zu bytes, received: %zd bytes.\n", */
            /*         expected_num_bytes, num_bytes); */
            continue;
        }
        ++iter_receiver;
        total_bits_received += packet_size;
        /* if (memcmp(&sock_addr, &sock_addr_backup, sizeof(sock_addr)) != 0) */
        /*     puts("sock_addr has been changed..."); */

        size_t iter_sender;
        memcpy(&iter_sender, recv_buf + sizeof(struct ether_header),
                sizeof(iter_sender));
        if (iter_sender != iter_receiver) {
            assert(iter_sender > iter_receiver);
            size_t cur_diff = iter_sender - iter_receiver;
            total_bits_lost += cur_diff * packet_size;
            size_t total_bits_sent = iter_sender * packet_size;
            double lost_percent = (double)(total_bits_lost * 100) / (double)total_bits_sent;

            printf("Sender iteration: %zu, receiver iteration: %zu, cur_diff = %zu, total_bits_lost = %zu (%f%%)\n",
                    iter_sender, iter_receiver, cur_diff, total_bits_lost, lost_percent);
            iter_receiver = iter_sender;
            /* close(sockfd); */
            /* return 7; */
        }

        /* usleep(100000); */
        if ((iter_receiver % 1000) == 0)
            printf("Bits received: %zu\n", total_bits_received);
    }

    /* close(sockfd); */
    /* return 0; */
}
