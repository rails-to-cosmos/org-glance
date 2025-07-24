FROM silex/emacs:29.1

# Install eask
RUN curl -SsL -o /etc/apt/trusted.gpg.d/easksource.gpg https://raw.githubusercontent.com/emacs-eask/packaging/master/debian/KEY.gpg
RUN curl -SsL -o /etc/apt/sources.list.d/easksource.list https://raw.githubusercontent.com/emacs-eask/packaging/master/debian/easksource.list
RUN apt update --allow-insecure-repositories
RUN apt install eask-cli --allow-unauthenticated
