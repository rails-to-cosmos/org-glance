FROM silex/emacs:29

WORKDIR /workspace

RUN apt update && apt install -y npm

RUN npm install -g @emacs-eask/cli

COPY . .

RUN eask install
