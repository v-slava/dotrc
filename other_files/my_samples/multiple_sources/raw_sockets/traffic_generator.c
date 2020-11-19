/* EVAL REGION BEGINS HERE: |* |
 * let g:My_eval_var = "MyRunShellCmd clang -g3 -Weverything -pedantic
 * \ -pthread traffic_generator.c -o ~/my/traffic_generator"
 * EVAL REGION ENDS HERE. */

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <stdbool.h>
#include <stdatomic.h>
#include <time.h>
#include <assert.h>
#include <errno.h>
#include <signal.h>
#include <unistd.h>
#include <pthread.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <sys/ioctl.h>
#include <net/if.h>
#include <linux/if_packet.h>
#include <netinet/ether.h>

#define NO_RETURN __attribute__((noreturn))

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpadded"
struct cmd_line_args {
    size_t payload_size;
    unsigned time_sec;
    unsigned char dest_mac[ETH_ALEN /* = 6 */];
    bool need_to_send;
    const char *interface_name;
};

struct context {
    struct cmd_line_args args;
    struct timespec initial_time;
    pthread_t sender_thread;
    pthread_t receiver_thread;
    _Atomic bool stop_flag;
};
#pragma GCC diagnostic pop

static struct context global_context = {
    .stop_flag = false,
};

static bool get_stop_flag(void)
{
    return atomic_load_explicit(&global_context.stop_flag,
                                memory_order_relaxed);
}

static void set_stop_flag(void)
{
    atomic_store_explicit(&global_context.stop_flag, true,
                          memory_order_relaxed);
}

static NO_RETURN void print_help(int exit_code, const char *argv0)
{
    assert(argv0 != NULL);
    FILE *stream = exit_code ? stderr : stdout;
    fprintf(stream, "Usage: %s [-h] [-s DEST_MAC] -i INTERFACE_NAME -t SECONDS \
-p PAYLOAD_SIZE\n\nWe always receive, send only if \"-s\" is present.\n",
            argv0);
    exit(exit_code);
}

static void parse_cmd_line_args(int argc, char *argv[],
                                struct cmd_line_args *args)
{
    assert(argv != NULL);
    assert(args != NULL);
    enum args_list {
        ARG_INTERFACE_NAME,
        ARG_PAYLOAD_SIZE,
        ARG_TIME,
        // Add new arguments before this line.
#ifdef __clang__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wduplicate-enum"
#endif
        ARG_LAST,
        ALL_ARGS_PRESENT = (unsigned)((1ull << ARG_LAST) - 1),
#ifdef __clang__
#pragma GCC diagnostic pop
#endif
    };
    unsigned args_present = 0;
    static_assert(ARG_LAST < sizeof(args_present) * CHAR_BIT, "Too many args");
    bool help_requested = false;

    static const char* opt_string = "+hi:p:s:t:";
    bool have_more_args = true;
    do {
        int ret = getopt(argc, argv, opt_string);
        switch (ret)
        {
        case -1:
            if (optind != argc) {
                fprintf(stderr, "Got unexpected command line argumet: %s\n",
                        argv[optind]);
                exit(EXIT_FAILURE);
            }
            have_more_args = false;
            break;
        case 'h':
            help_requested = true;
            break;
        case 'i':
            if (strlen(optarg) >= IFNAMSIZ) {
                fprintf(stderr, "Got too long interface name: %s\n", optarg);
                exit(EXIT_FAILURE);
            }
            args->interface_name = optarg;
            args_present |= (1u << ARG_INTERFACE_NAME);
            break;
        case 'p': {
            char *endptr;
            long val = strtol(optarg, &endptr, 10);
            if ((val < 0) || (val == LONG_MAX) || (*endptr != 0)) {
                fprintf(stderr, "Got invalid payload size: %s\n", optarg);
                exit(EXIT_FAILURE);
            }
            args->payload_size = (size_t)val;
            args_present |= (1u << ARG_PAYLOAD_SIZE);
            } break;
        case 's': {
            args->need_to_send = true;
            size_t pos = 0;
            for (int i = 0; i < 6; i++) {
                char *endptr;
                long val = strtol(optarg + pos, &endptr, 16);
                if ((val < 0) || (val > 255) || (endptr != optarg + pos + 2)
                        || ((*endptr != ':') && (*endptr != 0))) {
                    fprintf(stderr, "Got invalid destination MAC: %s\n",
                            optarg);
                    exit(EXIT_FAILURE);
                }
                pos += 3;
                args->dest_mac[i] = (unsigned char)val;
            }
            } break;
        case 't': {
            char *endptr;
            long val = strtol(optarg, &endptr, 10);
            if ((val < 0) || (val == LONG_MAX) || (*endptr != 0)) {
                fprintf(stderr, "Got invalid time: %s\n", optarg);
                exit(EXIT_FAILURE);
            }
            args->time_sec = (unsigned)val;
            args_present |= (1u << ARG_TIME);
            } break;
        case '?': // Got unknown option or missing option argument.
            // By now getopt() should have already printed error message to
            // stderr, so nothing to do anymore.
            exit(EXIT_FAILURE);
        default:
            fprintf(stderr, "getopt() returned unexpected value: '%c' (0x%X)\n",
                    ret, ret);
            exit(EXIT_FAILURE);
        }
    } while (have_more_args);

    if (help_requested) {
        int exit_code = (args_present == 0) ? EXIT_SUCCESS : EXIT_FAILURE;
        print_help(exit_code, argv[0]);
    }

    if (args_present != ALL_ARGS_PRESENT) {
        fputs("Error: some obligatory command line arguments are missing\n",
              stderr);
        exit(EXIT_FAILURE);
    }
}

// This function is executed when user presses CTRL-C
static void sigint_handler(int signal)
{
    assert(signal == SIGINT);
    set_stop_flag();
}

static void register_signal_handler(int signal, void (*handler)(int))
{
    assert(handler != NULL);
    struct sigaction sa;
#ifdef __clang__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdisabled-macro-expansion"
#endif
    sa.sa_handler = handler;
#ifdef __clang__
#pragma GCC diagnostic pop
#endif
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;
    errno = 0;
    int ret = sigaction(signal, &sa, NULL);
    if (ret != 0) {
        perror("sigaction");
        exit(EXIT_FAILURE);
    }
}

static void get_time(struct timespec *ts)
{
    assert(ts != NULL);
    errno = 0;
    int ret = clock_gettime(CLOCK_MONOTONIC_COARSE, ts);
    if (ret != 0) {
        perror("clock_gettime");
        exit(EXIT_FAILURE);
    }
}

static void handle_pthread(int ret, const char *func)
{
    assert(func != NULL);
    if (ret != 0) {
        fprintf(stderr, "pthread_%s() failed: %s\n", func, strerror(ret));
        exit(EXIT_FAILURE);
    }
}

static void create_thread(void *(*start_func)(void *), pthread_t *thread,
                          void *context)
{
    assert(start_func != NULL);
    assert(thread != NULL);
    pthread_attr_t attr;
    handle_pthread(pthread_attr_init(&attr), "attr_init");
    handle_pthread(pthread_create(thread, &attr, start_func, context),
                   "create");
    handle_pthread(pthread_attr_destroy(&attr), "attr_destroy");
}

static void wait_for_thread_termination(pthread_t *thread)
{
    void *ret_val;
    assert(thread != NULL);
    handle_pthread(pthread_join(*thread, &ret_val), "join");
    assert((size_t)ret_val == EXIT_SUCCESS);
}

static void do_sleep(unsigned num_seconds)
{
    unsigned sec_left = num_seconds;
    while (sec_left != 0)
        sec_left = sleep(sec_left);
}

static void *do_receive(void *c)
{
    struct context *ctx = (struct context *)c;
    unsigned char recv_buf[10000];
    size_t payload_size = ctx->args.payload_size;
    //                       (     6 + 6 + 2     )
    size_t recv_size = sizeof(struct ether_header) + payload_size;
    size_t packet_size = ((7 + 1) + recv_size + (4 + 8)) * 8;
    if (recv_size > sizeof(recv_buf)) {
        fprintf(stderr, "Payload size is too big: %zu\n", payload_size);
        exit(EXIT_FAILURE);
    }

    // ETH_P_IP = 0x0800 = IPv4, IPPROTO_RAW = 255
    unsigned short ether_type = ETH_P_ALL /* 0x0003 */;

    errno = 0;
    int sockfd = socket(AF_PACKET, SOCK_RAW, htons(ether_type)); // see raw(7)
    if (sockfd == -1) {
        perror("socket");
        exit(EXIT_FAILURE);
    }

    struct ifreq ifr; // See netdevice (7).
    memset(&ifr, 0, sizeof(ifr));
    // The following is guaranteed by parse_cmd_line_args():
    assert(sizeof(ifr.ifr_name) > strlen(ctx->args.interface_name));
    strcpy(ifr.ifr_name, ctx->args.interface_name);
    errno = 0;
    int ret = ioctl(sockfd, SIOCGIFINDEX, &ifr);
    if (ret < 0) {
        perror("ioctl(SIOCGIFINDEX)");
        close(sockfd);
        exit(EXIT_FAILURE);
    }

    struct sockaddr_ll sock_addr; // See packet (7)
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
        exit(EXIT_FAILURE);
    }

// #define SET_TO_PROMISC_MODE
#ifdef SET_TO_PROMISC_MODE
    // Set the network card in promiscuos mode: instructs network card to
    // forward to CPU all frames, not only frames with
    // destination MAC == our MAC.
    memset(&ifr, 0, sizeof(ifr));
    strcpy(ifr.ifr_name, ctx->args.interface_name);
    errno = 0;
    ret = ioctl(sockfd, SIOCGIFFLAGS, &ifr);
    if (ret < 0) {
        perror("ioctl(SIOCGIFFLAGS)");
        close(sockfd);
        exit(EXIT_FAILURE);
    }

    ifr.ifr_flags |= IFF_PROMISC;

    errno = 0;
    ret = ioctl(sockfd, SIOCSIFFLAGS, &ifr);
    if (ret < 0) {
        perror("ioctl(SIOCSIFFLAGS)");
        close(sockfd);
        exit(EXIT_FAILURE);
    }
#endif // SET_TO_PROMISC_MODE

    unsigned char my_mac[ETH_ALEN];
    memset(&ifr, 0, sizeof(ifr));
    strcpy(ifr.ifr_name, ctx->args.interface_name);
    errno = 0;
    ret = ioctl(sockfd, SIOCGIFHWADDR, &ifr);
    if (ret < 0) {
        perror("ioctl(SIOCGIFHWADDR)");
        close(sockfd);
        exit(EXIT_FAILURE);
    }
    memcpy(my_mac, &ifr.ifr_hwaddr.sa_data, ETH_ALEN);

    size_t iter_receiver = 0, total_bits_lost = 0, total_bits_received = 0;
    struct ether_header *eh = (struct ether_header *)recv_buf;
    puts("Start receiving...");
    struct timespec start, end;
    while (true) {
        get_time(&end);
        assert(end.tv_sec >= ctx->initial_time.tv_sec);
        if (end.tv_sec - ctx->initial_time.tv_sec >= ctx->args.time_sec - 1)
            break;
        if (get_stop_flag())
            break;

        int flags = MSG_DONTWAIT; // 0
        memset(&sock_addr, 0, sizeof(sock_addr));
        socklen_t addrlen = sizeof(sock_addr);
        errno = 0;
        ssize_t num_bytes = recvfrom(sockfd, recv_buf, sizeof(recv_buf), flags,
                               (struct sockaddr*)&sock_addr, &addrlen);
        if (num_bytes < 0) {
            if ((errno == EAGAIN) || (errno == EWOULDBLOCK))
                continue;
            perror("recvfrom");
            close(sockfd);
            exit(EXIT_FAILURE);
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

        // Skip packets from do_send():
        if (sock_addr.sll_pkttype == PACKET_OUTGOING)
            continue;

        if ((size_t)num_bytes != recv_size) {
            /* printf("Expected: %zu bytes, received: %zd bytes.\n", */
            /*         expected_num_bytes, num_bytes); */
            continue;
        }
        // if (memcmp(eh->ether_dhost, my_mac, ETH_ALEN) != 0)
        //     continue; // Wrong destination MAC -> ignore

        if (iter_receiver == 0) {
            get_time(&start);
            assert(start.tv_sec >= ctx->initial_time.tv_sec);
        }

        ++iter_receiver;
        total_bits_received += packet_size;
        /* if (memcmp(&sock_addr, &sock_addr_backup, sizeof(sock_addr)) != 0) */
        /*     puts("sock_addr has been changed..."); */

        size_t iter_sender;
        memcpy(&iter_sender, recv_buf + sizeof(struct ether_header),
                sizeof(iter_sender));

        if (iter_sender != iter_receiver) {
            // The following is not true for overflow, but we don't expect to
            // run the program for so long to be able to overflow it.
            assert(iter_sender > iter_receiver);

            size_t cur_diff = iter_sender - iter_receiver;
            total_bits_lost += cur_diff * packet_size;
            size_t total_bits_sent = iter_sender * packet_size;
            double lost_x100 = (double)(total_bits_lost * 100);
            double lost_percent = lost_x100 / (double)total_bits_sent;

            printf("Receiver: sender iteration = %zu, receiver iteration = %zu,"
                    " cur_diff = %zu, total_bits_lost = %zu (%f%%)\n",
                    iter_sender, iter_receiver, cur_diff, total_bits_lost,
                    lost_percent);
            iter_receiver = iter_sender;
            /* close(sockfd); */
            /* return (void*)EXIT_FAILURE; */
        }

        /* usleep(100000); */
        if ((iter_receiver % 10000) == 0)
            printf("Receiver: bits received: %zu\n", total_bits_received);
    }

    unsigned long long start_ns = (unsigned long long)start.tv_sec
                            * 1000000000ull + (unsigned long long)start.tv_nsec;
    unsigned long long end_ns = (unsigned long long)end.tv_sec
                              * 1000000000ull + (unsigned long long)end.tv_nsec;
    unsigned long long delta_ns = end_ns - start_ns;
    unsigned long long throughput_bits_per_sec = total_bits_received
                                                 * 1000000000ull / delta_ns;

    size_t total_bits_sent = iter_receiver * packet_size;
    double lost_x100 = (double)(total_bits_lost * 100);
    double lost_percent = lost_x100 / (double)total_bits_sent;
    printf("\nReceiver: total bits: sent %zu, received %zu, lost %zu"
           " (%f%%), throughput %llu bits/sec\n\n", total_bits_sent,
           total_bits_received, total_bits_lost, lost_percent,
           throughput_bits_per_sec);

    close(sockfd);
    return NULL;
}

static void *do_send(void *c)
{
    struct context *ctx = (struct context *)c;
    unsigned char send_buf[10000];
    size_t payload_size = ctx->args.payload_size;
    //                       (     6 + 6 + 2     )
    size_t send_size = sizeof(struct ether_header) + payload_size;
    size_t packet_size = ((7 + 1) + send_size + (4 + 8)) * 8;
    if (send_size > sizeof(send_buf)) {
        fprintf(stderr, "Payload size is too big: %zu\n", payload_size);
        exit(EXIT_FAILURE);
    }

    unsigned short ether_type = ETH_P_ALL /* 0x0003 */;
    errno = 0;
    int sockfd = socket(AF_PACKET, SOCK_RAW, htons(ether_type)); // see raw(7)
    if (sockfd == -1) {
        perror("socket");
        exit(EXIT_FAILURE);
    }

    struct ifreq ifr; // See netdevice (7).
    memset(&ifr, 0, sizeof(ifr));
    // The following is guaranteed by parse_cmd_line_args():
    assert(sizeof(ifr.ifr_name) > strlen(ctx->args.interface_name));
    strcpy(ifr.ifr_name, ctx->args.interface_name);
    // Get ifr.ifr_ifindex from ifr.ifr_name. See netdevice (7):
    errno = 0;
    int ret = ioctl(sockfd, SIOCGIFINDEX, &ifr);
    if (ret < 0) {
        perror("ioctl(SIOCGIFINDEX)");
        close(sockfd);
        exit(EXIT_FAILURE);
    }

    struct sockaddr_ll sock_addr; // See packet (7).
    memset(&sock_addr, 0, sizeof(sock_addr));
    sock_addr.sll_family = AF_PACKET; // = PF_PACKET
    sock_addr.sll_protocol = htons(ether_type);
    sock_addr.sll_ifindex = ifr.ifr_ifindex;
    sock_addr.sll_halen = ETH_ALEN;
    memcpy(sock_addr.sll_addr, ctx->args.dest_mac, ETH_ALEN);

    memset(&ifr, 0, sizeof(ifr));
    strcpy(ifr.ifr_name, ctx->args.interface_name);
    errno = 0;
    ret = ioctl(sockfd, SIOCGIFHWADDR, &ifr);
    if (ret < 0) {
        perror("ioctl(SIOCGIFHWADDR)");
        close(sockfd);
        exit(EXIT_FAILURE);
    }

    memset(send_buf, 0, sizeof(send_buf));
    struct ether_header *eh = (struct ether_header *)send_buf;

    memcpy(eh->ether_shost, &ifr.ifr_hwaddr.sa_data, ETH_ALEN);
    memcpy(eh->ether_dhost, ctx->args.dest_mac, ETH_ALEN);
    eh->ether_type = 0xFFFF; // ETH_P_IP

    size_t tx_len = sizeof(*eh);
    for (size_t i = 1; i <= ctx->args.payload_size; i++) {
        send_buf[tx_len++] = (unsigned char)i;
    }
    memset(send_buf + sizeof(*eh), 0, 16);

    size_t iter = 0;
    puts("Start sending...");
    while (true) {
        struct timespec ts;
        get_time(&ts);
        assert(ts.tv_sec >= ctx->initial_time.tv_sec);
        if (ts.tv_sec - ctx->initial_time.tv_sec >= ctx->args.time_sec)
            break;
        if (get_stop_flag())
            break;
        ++iter;
        memcpy(send_buf + sizeof(*eh), &iter, sizeof(iter));

        int flags = 0;
        errno = 0;
        ssize_t num_bytes = sendto(sockfd, send_buf, tx_len, flags,
                               (struct sockaddr*)&sock_addr, sizeof(sock_addr));
        if (num_bytes < 0) {
            perror("sendto");
            close(sockfd);
            exit(EXIT_FAILURE);
        }

        if ((size_t)num_bytes != tx_len) {
            printf("Sender: attempted to send: %zu, actually sent: %zd\n",
                    tx_len, num_bytes);
        }

        if ((iter % 10000) == 0)
            printf("Sender: bits sent = %zu\n", iter * packet_size);
    }

    printf("\nSender: total bits sent = %zu\n\n", iter * packet_size);
    close(sockfd);
    return NULL;
}

int main(int argc, char *argv[])
{
    struct context *ctx = &global_context;
    parse_cmd_line_args(argc, argv, &ctx->args);
    register_signal_handler(SIGINT, sigint_handler);
    get_time(&ctx->initial_time);
    puts("Starting receiver thread...");
    create_thread(do_receive, &ctx->receiver_thread, ctx);
    if (ctx->args.need_to_send) {
        // This gives us time to start receiver thread on another PC.
        do_sleep(1);
        puts("Starting sender thread...");
        create_thread(do_send, &ctx->sender_thread, ctx);
        wait_for_thread_termination(&ctx->sender_thread);
        puts("Sender thread has been stopped.");
    }
    wait_for_thread_termination(&ctx->receiver_thread);
    puts("Receiver thread has been stopped.");
    return 0;
}
