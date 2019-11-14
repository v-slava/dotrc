#include <linux/cdrom.h>
#include <fcntl.h>

int main()
{
	int fd = open("/dev/sr0", O_RDWR|O_NONBLOCK);
	ioctl(fd, CDROMEJECT, 0);
	/* ioctl(fd, 0x5309, 0); */
	return 0;
}
