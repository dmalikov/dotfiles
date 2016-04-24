{
  networking.firewall.allowedTCPPorts = [ 80 443 ];

  services.nginx = {
    enable = true;

    config = ''
      worker_processes 2;
      events {
        worker_connections 2048;
      }
    '';

    httpConfig = ''
      sendfile on;
      tcp_nopush on;
      tcp_nodelay on;
      keepalive_timeout 65;
      types_hash_max_size 2048;

      gzip on;
      gzip_disable "msie6";

      limit_req_zone $binary_remote_addr zone=mysitelimit:10m rate=100r/s;

      server {
        listen 80;
        return 301 https://$host$request_uri;
      }

      server {
        server_name nuda.space;

        listen 443 ssl;
        listen [::]:80 ipv6only=on;

        ssl_certificate /etc/letsencrypt/live/nuda.space/fullchain.pem;
        ssl_certificate_key /etc/letsencrypt/live/nuda.space/privkey.pem;

        ssl_protocols TLSv1 TLSv1.1 TLSv1.2;
        ssl_prefer_server_ciphers on;
        ssl_dhparam /etc/ssl/certs/dhparam.pem;
        ssl_ciphers 'ECDHE-RSA-AES128-GCM-SHA256:ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-AES256-GCM-SHA384:DHE-RSA-AES128-GCM-SHA256:DHE-DSS-AES128-GCM-SHA256:kEDH+AESGCM:ECDHE-RSA-AES128-SHA256:ECDHE-ECDSA-AES128-SHA256:ECDHE-RSA-AES128-SHA:ECDHE-ECDSA-AES128-SHA:ECDHE-RSA-AES256-SHA384:ECDHE-ECDSA-AES256-SHA384:ECDHE-RSA-AES256-SHA:ECDHE-ECDSA-AES256-SHA:DHE-RSA-AES128-SHA256:DHE-RSA-AES128-SHA:DHE-DSS-AES128-SHA256:DHE-RSA-AES256-SHA256:DHE-DSS-AES256-SHA:DHE-RSA-AES256-SHA:AES128-GCM-SHA256:AES256-GCM-SHA384:AES128-SHA256:AES256-SHA256:AES128-SHA:AES256-SHA:AES:CAMELLIA:DES-CBC3-SHA:!aNULL:!eNULL:!EXPORT:!DES:!RC4:!MD5:!PSK:!aECDH:!EDH-DSS-DES-CBC3-SHA:!EDH-RSA-DES-CBC3-SHA:!KRB5-DES-CBC3-SHA';
        ssl_session_timeout 1d;
        ssl_session_cache shared:SSL:50m;
        ssl_stapling on;
        ssl_stapling_verify on;
        add_header Strict-Transport-Security max-age=15768000;

        root /srv/nuda.space;
        index index.html index.htm;

        location / {
          try_files $uri $uri/ /index.html;
          proxy_pass http://127.0.0.1:8081$request_uri;
        }

        location ~ /.well-known {
            allow all;
        }

        # this prevents hidden files (beginning with a period) from being served
        location ~ /\. {
          access_log off;
          log_not_found off;
          deny all;
        }
      }
    '';
  };
}
