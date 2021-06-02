(define-module (services bluetooth)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu services dbus)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:export
  (bluetooth-service-type
   bluetooth-service))

(define (bool value)
  (if value "true\n" "false\n"))

(define-record-type* <bluetooth-configuration>
  bluetooth-configuration make-bluetooth-configuration
  bluetooth-configuration?
  (bluez bluetooth-configuration-bluez (default bluez))
  (auto-enable? bluetooth-configuration-auto-enable? (default #f)))

(define (bluetooth-configuration-file config)
  "Return a configuration file for the systemd bluetooth service, as a string."
  (string-append
   "[General]\n"
   "Enable=Source\n"
   "\n"
   "[Policy]\n"
   "AutoEnable=" (bool (bluetooth-configuration-auto-enable?
                        config))))

(define (bluetooth-directory config)
  (computed-file "etc-bluetooth"
                 #~(begin
                     (mkdir #$output)
                     (chdir #$output)
                     (call-with-output-file "main.conf"
                       (lambda (port)
                         (display #$(bluetooth-configuration-file config)
                                  port))))))

(define (bluetooth-shepherd-service config)
  "Return a shepherd service for @command{bluetoothd}."
  (shepherd-service
   (provision '(bluetooth))
   (requirement '(dbus-system udev))
   (documentation "Run the bluetoothd daemon.")
   (start #~(make-forkexec-constructor
             (list #$(file-append (bluetooth-configuration-bluez config)
                                  "/libexec/bluetooth/bluetoothd"))))
   (stop #~(make-kill-destructor))))

(define bluetooth-service-type
  (service-type
   (name 'bluetooth)
   (extensions
    (list (service-extension dbus-root-service-type
                             (compose list bluetooth-configuration-bluez))
          (service-extension udev-service-type
                             (compose list bluetooth-configuration-bluez))
          (service-extension etc-service-type
                             (lambda (config)
                               `(("bluetooth"
                                  ,(bluetooth-directory config)))))
          (service-extension shepherd-root-service-type
                             (compose list bluetooth-shepherd-service))))
   (default-value (bluetooth-configuration))
   (description "Run the @command{bluetoothd} daemon, which manages all the
Bluetooth devices and provides a number of D-Bus interfaces.")))

(define* (bluetooth-service #:key (bluez bluez) (auto-enable? #f))
  "Return a service that runs the @command{bluetoothd} daemon, which manages
all the Bluetooth devices and provides a number of D-Bus interfaces.  When
AUTO-ENABLE? is true, the bluetooth controller is powered automatically at
boot.

Users need to be in the @code{lp} group to access the D-Bus service.
"
  (service bluetooth-service-type
           (bluetooth-configuration
            (bluez bluez)
            (auto-enable? auto-enable?))))
