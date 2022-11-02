resource "google_compute_instance" "hydra-demo" {
  name         = "hydra-demo-1"

  # https://cloud.google.com/compute/docs/compute-optimized-machines
  machine_type = "c2d-standard-2"
  allow_stopping_for_update = true

  tags = [ "hydra", "testnet" ]

  metadata = {
    sshKeys = file("ssh_keys")
  }

  boot_disk {
    initialize_params {
      size  = 50
      image = "iog-hydra-1665828710"
    }
  }

  network_interface {
    network       = "default"
    access_config {
      nat_ip = google_compute_address.hydra-demo-address.address
    }
  }

  connection {
    type = "ssh"
    user = "curry"
    host = self.network_interface.0.access_config.0.nat_ip
  }

  provisioner "file" {
    source      = "scripts/configure-testnet.sh"
    destination = "/home/curry/configure-testnet.sh"
  }

  provisioner "file" {
    source      = "scripts/fuel-testnet.sh"
    destination = "/home/curry/fuel-testnet.sh"
  }

  provisioner "remote-exec" {
    inline = [
      "mkdir keys"
    ]
  }

  provisioner "file" {
    source      = "keys/"
    destination = "/home/curry/keys"
  }

  provisioner "file" {
    source      = "prometheus.yml"
    destination = "/home/curry/prometheus.yml"
  }

  provisioner "file" {
    source      = "../../hydra-cluster/config/devnet/genesis-shelley.json"
    destination = "/home/curry/genesis-shelley.json"
  }

  provisioner "file" {
    source      = "../../hydra-cluster/config/devnet/genesis-alonzo.json"
    destination = "/home/curry/genesis-alonzo.json"
  }

  provisioner "file" {
    source      = "../../hydra-cluster/config/protocol-parameters.json"
    destination = "/home/curry/protocol-parameters.json"
  }


  provisioner "file" {
    source      = "docker-compose.yaml"
    destination = "/home/curry/docker-compose.yaml"
  }

  provisioner "file" {
    source      = "promtail-config.yml"
    destination = "/home/curry/promtail-config.yml"
  }

  provisioner "remote-exec" {
    inline = [
      "chmod +x /home/curry/configure-testnet.sh",
      "/home/curry/configure-testnet.sh"
    ]
  }

}

resource "google_compute_address" "hydra-demo-address" {
  name = "hydra-demo-address"
}

output "hydra-demo-ip" {
  value = google_compute_address.hydra-demo-address.address
}

output "project" {
  value = google_compute_instance.hydra-demo.project
}
