(use-modules (config ada))
(use-modules (config mel))
(use-modules (config seshat))

(list (machine
       (operating-system seshat-system-config)
       (environment managed-host-environment-type)
       (configuration
        (machine-ssh-configuration
         (host-name "coroot.de")
         (system "x86_64-linux")
         (identity "/home/robin/.ssh/id_ed25519")
         (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHDeKweSXGxPW8MQvynT2tN19M5ttMDPiGeGGg4Cbic2"))))
      (machine
       (operating-system mel-system-config)
       (environment managed-host-environment-type)
       (configuration
        (machine-ssh-configuration
         (allow-downgrades? #t)
         (host-name "192.168.3.4")
         (system "x86_64-linux")
         (identity "/home/robin/.ssh/id_ed25519")
         (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAII5NlwiXunTqXKa72M3Sa4wy0yDwdG7+lA9eJBBTTDlN"))))
      (machine
       (operating-system ada-system-config)
       (environment managed-host-environment-type)
       (configuration
        (machine-ssh-configuration
         (host-name "localhost")
         (system "x86_64-linux")
         (identity "/home/robin/.ssh/id_ed25519")
         (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM/4xQf0tckYwpdoarydve8MTpOjBdOOmMuaSb3K7sUv")))))
