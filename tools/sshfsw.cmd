# Mount remote folder via sshfs on the Windows system.
# The WinFsp have to be installed in the system, and the SSHFS-Win in the path
# Use the windows cmd file to avoid the confliction between the ssh bound to sshfs and the ssh in cygwin.
# An example to start this script with in background:
# $ cygstart.exe --hide <PATH>/sshfs.cmd user@host: Z: -ovolname=v36 -oIdentityFile="<PATH>/.ssh/id_rsa"
set PATH=C:\temp\SSHFS-Win\bin;%PATH%
FOR /L %%L IN (0,0,1) DO (
                            sshfs %*^
                            -odebug -ologlevel=debug1 -oStrictHostKeyChecking=no -oUserKnownHostsFile=/dev/null^
                            -oidmap=user -ouid=-1 -ogid=-1 -oumask=000 -ocreate_umask=000 -omax_readahead=1GB -oallow_other^
                            -olarge_read -okernel_cache -ofollow_symlinks -oPreferredAuthentications=publickey
                            ping -n 5 127.0.0.1 ^
                        )

