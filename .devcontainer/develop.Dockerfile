ARG CLEAN_DATE

FROM itasks/clean:bundle-complete-$CLEAN_DATE

RUN \
	apt-get update -qq && \
	apt-get upgrade -qq && \
	apt-get install -qq --no-install-recommends \
		build-essential \
		git \
		git-lfs \
		python3-dev \
		python3-pip \
		python3-setuptools \
		vim \
		zsh && \
	pip3 install thefuck && \
	apt-get autoremove -qq && \
	apt-get autoclean -qq && \
	rm -rf /var/lib/apt/lists/*

# Install git-lfs
RUN \
	git lfs install

RUN \
	# Install oh-my-zsh with prompts removed.
	sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)" --unattended

RUN \
	# Install oh-my-zsh autosuggestion plugin (needs to be done in a different way than other plugins
	# As it is developed by users.). Same for syntax-highlighting. The debian packages
	# did not work.
	git clone https://github.com/zsh-users/zsh-autosuggestions ~/.oh-my-zsh/plugins/zsh-autosuggestions && \
	git clone https://github.com/zsh-users/zsh-syntax-highlighting ~/.oh-my-zsh/plugins/zsh-syntax-highlighting && \
	git clone https://github.com/tymm/zsh-directory-history ~/.oh.my-zsh/plugins/zsh-directory-history

# Copy the preprepared zsh file to the zshrc file of the container.
COPY .devcontainer/.zshrc /.zshrc

# Replace the default zshrc file with the preprepared file.
RUN \
	mv /.zshrc ~/.zshrc
