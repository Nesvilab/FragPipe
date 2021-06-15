## Setting up FragPipe on remote Linux server (with X forwarding)

Users can run [FragPipe](https://fragpipe.nesvilab.org/) on a remote Linux server with X forwarding. Both of the server and client need to be setup.

__For the details of setting up FragPipe itself, please refer to [Setting up FragPipe](https://fragpipe.nesvilab.org/docs/tutorial_setup_fragpipe.html).__

*Disclaimer: different Linux distributions have different ways to install and setup X forwarding. Thus, instructions below may need to be modified for a particular system.*

### Setup the remote Linux server
1. The server needs to have X window related packages. Different distributions have different ways to install those package. Following is an example for Ubuntu:
```shell
sudo apt-get update
sudo apt-get upgrade
sudo apt-get install xorg openbox
```
2. Open `/etc/ssh/sshd_config` (need root permission), and edit:
```shell
X11Forwarding yes
X11DisplayOffset 10
```
3. Restart ssh daemon using `sudo service sshd restart`


### Setup the Windows client on your Windows machine
1. Download PuTTY from [https://www.chiark.greenend.org.uk/~sgtatham/putty/latest.html](https://www.chiark.greenend.org.uk/~sgtatham/putty/latest.html), and install it.
2. Download Xming from [https://sourceforge.net/projects/xming/](https://sourceforge.net/projects/xming/), and install it.
3. Make sure that Xming is running in the background. Start Xming by clicking `Xming` from the Start menu:

<img src="https://raw.githubusercontent.com/Nesvilab/MSFragger/master/images/xming.jpg" width="200px" align="middle"/>

4. Open PuTTY, fill in `Host Name (or IP address)` and `Saved Sessions`, then clicking `Save`.

<img src="https://raw.githubusercontent.com/Nesvilab/MSFragger/master/images/putty1.jpg" width="400px" align="middle"/>

5. Click `SSH` then `X11`. Then, click `Enable X11 forwarding`.

<img src="https://raw.githubusercontent.com/Nesvilab/MSFragger/master/images/putty2.jpg" width="400px" align="middle"/>

6. (Optional) If you are using private key to login. Click `SSH` then `Auth`. Then, fill your private key path in `Private key file for authentication`.

<img src="https://raw.githubusercontent.com/Nesvilab/MSFragger/master/images/putty3.jpg" width="400px" align="middle"/>

7. Go back to `Session` and click `Save` again to save your settings.

<img src="https://raw.githubusercontent.com/Nesvilab/MSFragger/master/images/putty4.jpg" width="400px" align="middle"/>

8. Click `Open` to log into your server with user name and password (or private key).

9. Find your FragPipe and start `fragpipe` in `<FragPipe directory>/bin`.

10. Enjoy!


### Setup the Mac client on your Mac machine
1. Download XQuartz from [https://www.xquartz.org/](https://www.xquartz.org/), and install it.
2. Run Applications -> Utilities -> XQuartz.app
3. Right click on the XQuartz icon in the dock and select Applications -> Terminal.
4. In the opened xterm window, log into the server using (replacing `username` and `ip-address` to real ones)
```shell
ssh -X username@ip-address
```
5. Find your FragPipe and start `fragpipe` in `<FragPipe directory>/bin`.
6. Enjoy!
