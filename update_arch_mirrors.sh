sudo cp /etc/pacman.d/mirrorlist /etc/pacman.d/mirrorlist.old
curl -s "https://archlinux.org/mirrorlist/?country=US&protocol=http&protocol=https&ip_version=4&use_mirror_status=on" | sed -e 's/^#Server/Server/' -e '/^#/d' | rankmirrors -n 6 - > ~/mirrorlist
sudo mv ~/mirrorlist /etc/pacman.d/mirrorlist
cat /etc/pacman.d/mirrorlist
cat /etc/pacman.d/mirrorlist.old
echo "----- running paccache -----"
paccache -r
