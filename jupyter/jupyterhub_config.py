# jupyterhub_config.py
# Status: Workshop at IGB, 2026-03-24
# Retrieved from server: Wed Mar 25 19:12:31 CET 2026

c = get_config()  #noqa
c.JupyterHub.authenticator_class = "shared-password"
c.SharedPasswordAuthenticator.user_password = "come-up-with-something"
c.Authenticator.allow_all = True
from dockerspawner import DockerSpawner
c.JupyterHub.spawner_class = DockerSpawner
c.DockerSpawner.allowed_images = {
    "Hydrographr image (workshop version)": "hydrographr-jupyter:workshop",
    "Hydrographr image (dev version)": "hydrographr-jupyter:dev",
}
c.DockerSpawner.volumes = {
    'jupyter-user-{username}': {
        'bind': '/home/participant/data_write',
        'mode': 'rw'
    },
    '/opt/jupyterhub/docker_workshop202603_oldversion/input_data/lakes': {
        'bind': '/home/participant/data_readonly/lakes',
        'mode': 'ro'
    }
}
c.DockerSpawner.notebook_dir = '/home/participant'
c.DockerSpawner.extra_host_config = {
    'extra_hosts': {
        'host.docker.internal': 'host-gateway'
    }
}
c.JupyterHub.base_url = '/workshop'
c.JupyterHub.public_url = 'https://livingdata.igb-berlin.de/workshop/'
c.JupyterHub.hub_routespec = '/workshop/hub/'
c.JupyterHub.hub_bind_url = 'http://0.0.0.0:8081'
c.JupyterHub.hub_connect_ip = '172.17.0.1'
c.DockerSpawner.hub_ip_connect = '127.17.0.1'
