#include <stdio.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <stdlib.h>
#include <netdb.h>
#include <string.h>
#include <unistd.h>

int create_tcp_socket();
void get_ip_string(char* ip_string, const char *host);
void init_remote_ip(const char* src, struct sockaddr_in* dst);
char *build_get_query(const char *host, const char *page);

#define PORT 80
#define USERAGENT "HTMLGET 1.0"
#define BUF_SIZE 50000

int main(void)
{
    const char *host = "65.19.140.5"; // "man.he.net"
    const char *page = "/"; // "index.html"

    int ret;
    int sock = create_tcp_socket();
    char ip_string[16];
    get_ip_string(ip_string, host);
    fprintf(stderr, "IP is %s\n", ip_string);
    struct sockaddr_in remote;
    remote.sin_family = AF_INET;
    init_remote_ip(ip_string, &remote);
    remote.sin_port = htons(PORT);

    if(connect(sock, (struct sockaddr *)&remote, sizeof(struct sockaddr)) < 0)
    {
        perror("Could not connect");
        exit(1);
    }
    char* get = build_get_query(host, page);

    fprintf(stderr, "Query is:\n<<START>>\n%s<<END>>\n", get);

    //Send the query to the server
    unsigned sent = 0;
    while(sent < strlen(get))
    {
        ret = send(sock, get + sent, strlen(get) - sent, 0);
        if(ret == -1)
        {
            perror("Can't send query");
            exit(1);
        }
        sent += ret;
    }
    //now it is time to receive the page
    char buf[BUF_SIZE + 1];
    memset(buf, 0, sizeof(buf));
    while((ret = recv(sock, buf, BUF_SIZE, 0)) > 0)
    {
        if(ret != 0)
        {
            printf("received %u bytes:\n", ret);
            printf("%s", buf);
        }
        memset(buf, 0, ret);
    }
    if(ret < 0)
    {
        perror("Error receiving data");
    }
    free(get);
    close(sock);
    return 0;
}

int create_tcp_socket()
{
    int sock;
    if((sock = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP)) < 0)
    {
        perror("Can't create TCP socket");
        exit(1);
    }
    return sock;
}

void get_ip_string(char* ip_string, const char *host)
{
    int iplen = 15; // XXX.XXX.XXX.XXX
    memset(ip_string, 0, iplen + 1);
    struct hostent *hent = gethostbyname(host);
    if(hent == NULL)
    {
        herror("Can't get IP");
        exit(1);
    }
    if(inet_ntop(AF_INET, (void *)hent->h_addr_list[0], ip_string, iplen) == NULL)
    {
        perror("Can't resolve host");
        exit(1);
    }
}

void init_remote_ip(const char* src, struct sockaddr_in* dst)
{
    int ret = inet_pton(AF_INET, src, (void *)(&(dst->sin_addr.s_addr)));
    if(ret < 0)
    {
        perror("Can't set remote->sin_addr.s_addr");
        exit(1);
    }
    else if(ret == 0)
    {
        fprintf(stderr, "%s is not a valid IP address\n", src);
        exit(1);
    }
}

char *build_get_query(const char *host, const char *page)
{
    char *query;
    const char *getpage = page;
    char *tpl = "GET /%s HTTP/1.0\r\nHost: %s\r\nUser-Agent: %s\r\n\r\n";
    if(getpage[0] == '/')
    {
        getpage = getpage + 1;
        /* fprintf(stderr, "Removing leading \"/\", converting %s to %s\n", page, getpage); */
    }
    // -5 is to consider the %s %s %s in tpl and the ending \0
    query = (char *)malloc(strlen(host) + strlen(getpage) + strlen(USERAGENT) + strlen(tpl) - 5);
    sprintf(query, tpl, getpage, host, USERAGENT);
    return query;
}
