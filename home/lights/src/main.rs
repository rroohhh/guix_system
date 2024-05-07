use rumqttc::{Client, MqttOptions, QoS};
use std::io::Write;
use std::thread;
use std::time::Duration;

fn deserialize_bool<'de, D>(deserializer: D) -> Result<bool, D::Error>
where
    D: serde::de::Deserializer<'de>,
{
    let s: &str = serde::de::Deserialize::deserialize(deserializer)?;

    match s {
        "ON" => Ok(true),
        "OFF" => Ok(false),
        _ => Err(serde::de::Error::unknown_variant(s, &["ON", "OFF"])),
    }
}

#[derive(serde::Deserialize, Debug)]
struct LightInfo {
    brightness: u8,
    color_temp: u16,
    #[serde(deserialize_with = "deserialize_bool")]
    state: bool,
}

#[derive(serde::Serialize, Debug)]
struct WaybarOutput {
    text: String,
    #[serde(skip_serializing_if = "String::is_empty")]
    alt: String,
    #[serde(skip_serializing_if = "String::is_empty")]
    tooltip: String,
    #[serde(skip_serializing_if = "String::is_empty")]
    class: String,
    #[serde(skip_serializing_if = "is_zero")]
    percentage: u8,
}

fn is_zero(num: &u8) -> bool {
    *num == 0
}

fn main() {
    use rand::{distributions::Alphanumeric, Rng};
    let s: String = rand::thread_rng()
        .sample_iter(&Alphanumeric)
        .take(7)
        .map(char::from)
        .collect();
    let mut mqttoptions = MqttOptions::new(format!("lights-{s}"), std::env::args().nth(1).unwrap(), 1883);
    mqttoptions.set_keep_alive(Duration::from_secs(5));

    let (mut client, mut connection) = Client::new(mqttoptions, 10);
    client
        .subscribe("zigbee2mqtt/robin/vorne 3", QoS::AtLeastOnce)
        .unwrap();
    thread::spawn(move || {
        loop {
        client
            .publish(
                "zigbee2mqtt/robin/vorne 3/get",
                QoS::ExactlyOnce,
                false,
                "{\"brightness\": \"\"}",
            )
            .unwrap();
        thread::sleep(Duration::from_secs(15));


        }
    });

    let thermo = ["", "", "", "", ""];
    let thermo_min = 250;
    let thermo_max = 454;
    let thermo_dist = thermo_max - thermo_min;

    let mut stdout = std::io::stdout();
    for notification in connection.iter() {
        match notification.unwrap() {
            rumqttc::Event::Incoming(inc) => match inc {
                rumqttc::Packet::Publish(p) => {
                    eprintln!("{:?}", std::str::from_utf8(&p.payload[..]).unwrap());
                    if let Ok(info) = serde_json::from_slice::<LightInfo>(&p.payload[..]) {
                        let brightness_percent = 100u16 * (info.brightness as u16) / 254;
                        let thermo_percent = 100 * (info.color_temp - thermo_min) / thermo_dist;
                        let thermo_symbol = thermo[thermo.len() * thermo_percent as usize / 101];
                        let out = WaybarOutput {
                            text: format!(
                                " {brightness_percent}% {thermo_symbol} {thermo_percent}%"
                            ),
                            alt: "".to_owned(),
                            tooltip: "".to_owned(),
                            class: (if info.state { "enabled" } else { "disabled" }).to_owned(),
                            percentage: 0,
                        };
                        serde_json::to_writer(&stdout, &out).unwrap();
                        println!("");
                        stdout.flush().unwrap();
                    }
                }
                _ => {}
            },
            _ => {}
        }
    }
}
