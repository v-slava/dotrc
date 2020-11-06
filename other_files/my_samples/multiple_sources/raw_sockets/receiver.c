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

/* #define MTU 46 */
#define MTU 1500
/* #define MTU 9000 */
#define BUF_SIZE (sizeof(struct ether_header) + MTU)
    unsigned char recvbuf[BUF_SIZE];

    size_t iter = 0, iter_compare = 0;
    while (1) {
        ++iter;

        int flags = 0;
        errno = 0;
        /* struct sockaddr_ll sock_addr; // See packet (7). */
        memset(&sock_addr, 0, sizeof(sock_addr));
        socklen_t addrlen = sizeof(sock_addr);
        ssize_t num_bytes = recvfrom(sockfd, recvbuf, sizeof(recvbuf), flags,
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
        if ((size_t)num_bytes != expected_num_bytes)
            printf("Expected: %zu bytes, received: %zd bytes.\n",
                    expected_num_bytes, num_bytes);
        /* if (memcmp(sendbuf, sendbuf_backup, tx_len) != 0) */
        /*     puts("sendbuf has been changed..."); */
        /* if (memcmp(&sock_addr, &sock_addr_backup, sizeof(sock_addr)) != 0) */
        /*     puts("sock_addr has been changed..."); */

        size_t iter_sender;
        memcpy(&iter_sender, recvbuf + sizeof(struct ether_header),
                sizeof(iter_sender));
        ++iter_compare;
        if (iter_sender != iter_compare) {
            assert(iter_sender > iter_compare);
            printf("Sender iteration: %zu, receiver iteration: %zu, diff = %zu\n",
                    iter_sender, iter_compare, iter_sender - iter_compare);
            iter_compare = iter_sender;
            /* close(sockfd); */
            /* return 7; */
        }

        /* if ((iter % 1000) == 0) { */
        /*     // printf("Iteration #%zu\n", iter); */
        /*     size_t packet_size = ((7 + 1) + (6 + 6 + 2) + MTU + (4 + 8)) * 8; */
        /*     printf("Bits received: %zu\n", iter * packet_size); */
        /* } */
    }

    /* close(sockfd); */
    /* return 0; */
}
