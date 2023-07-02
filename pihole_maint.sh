ssh pihole "pihole -a -t"
scp pihole:/home/pihole/pi-hole*.tar.gz ~/Dropbox/pihole_backup/
ssh pihole "rm pi-hole*.tar.gz"
ssh pihole "sudo apt update -y && sudo apt upgrade -y"
ssh pihole "pihole -up"
ssh pihole "sudo shutdown --reboot now"
