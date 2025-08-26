#!/usr/bin/env bash
set -euo pipefail

echo "🚀 PSYOP Digital Ocean Server Configuration"
echo "=========================================="

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to print colored output
print_status() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Function to check if command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Function to update system
update_system() {
    print_status "Updating system packages..."
    apt-get update -y
    apt-get upgrade -y
    print_success "System updated"
}

# Function to install essential packages
install_essential_packages() {
    print_status "Installing essential packages..."
    
    # Basic utilities
    apt-get install -y \
        curl \
        wget \
        git \
        vim \
        htop \
        tree \
        unzip \
        zip \
        jq \
        ripgrep \
        fd-find \
        bat \
        exa \
        fzf \
        tmux \
        zsh \
        ca-certificates \
        build-essential \
        libgmp-dev \
        libtinfo-dev \
        zlib1g-dev \
        xz-utils \
        libssl-dev \
        pkg-config \
        software-properties-common \
        apt-transport-https \
        gnupg \
        lsb-release
    
    print_success "Essential packages installed"
}

# Function to install Node.js and npm
install_nodejs() {
    if ! command_exists node; then
        print_status "Installing Node.js and npm..."
        
        # Install Node.js 20.x LTS
        curl -fsSL https://deb.nodesource.com/setup_20.x | bash -
        apt-get install -y nodejs
        
        # Install global npm packages
        npm install -g npm@latest
        npm install -g yarn pnpm
        
        print_success "Node.js and npm installed"
    else
        print_status "Node.js already installed: $(node --version)"
    fi
}

# Function to install Rust
install_rust() {
    if ! command_exists cargo; then
        print_status "Installing Rust..."
        curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
        
        # Source Rust environment
        source ~/.cargo/env
        
        print_success "Rust installed: $(cargo --version)"
    else
        print_status "Rust already installed: $(cargo --version)"
    fi
}

# Function to install Go
install_go() {
    if ! command_exists go; then
        print_status "Installing Go..."
        
        # Download latest Go
        GO_VERSION=$(curl -s https://golang.org/dl/ | grep -o 'go[0-9]\+\.[0-9]\+' | head -1)
        wget "https://golang.org/dl/${GO_VERSION}.linux-amd64.tar.gz"
        tar -C /usr/local -xzf "${GO_VERSION}.linux-amd64.tar.gz"
        rm "${GO_VERSION}.linux-amd64.tar.gz"
        
        # Add to PATH
        echo 'export PATH=$PATH:/usr/local/go/bin' >> ~/.bashrc
        echo 'export PATH=$PATH:/usr/local/go/bin' >> ~/.zshrc
        
        print_success "Go installed: $(/usr/local/go/bin/go version)"
    else
        print_status "Go already installed: $(go version)"
    fi
}

# Function to install Python tools
install_python_tools() {
    print_status "Installing Python tools..."
    
    # Install pip if not present
    if ! command_exists pip3; then
        apt-get install -y python3-pip
    fi
    
    # Install Python packages
    pip3 install --upgrade pip
    pip3 install \
        black \
        flake8 \
        mypy \
        pytest \
        ipython \
        jupyter \
        pandas \
        numpy \
        matplotlib \
        requests \
        beautifulsoup4 \
        lxml
    
    print_success "Python tools installed"
}

# Function to install Docker
install_docker() {
    if ! command_exists docker; then
        print_status "Installing Docker..."
        
        # Install Docker
        curl -fsSL https://get.docker.com -o get-docker.sh
        sh get-docker.sh
        
        # Install Docker Compose
        curl -L "https://github.com/docker/compose/releases/latest/download/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
        chmod +x /usr/local/bin/docker-compose
        
        # Add user to docker group
        usermod -aG docker $USER
        
        print_success "Docker installed: $(docker --version)"
    else
        print_status "Docker already installed: $(docker --version)"
    fi
}

# Function to install Neovim
install_neovim() {
    if ! command_exists nvim; then
        print_status "Installing Neovim..."
        
        # Add Neovim PPA
        add-apt-repository ppa:neovim-ppa/stable -y
        apt-get update
        
        # Install Neovim
        apt-get install -y neovim
        
        # Install Python support
        pip3 install pynvim
        
        print_success "Neovim installed: $(nvim --version | head -1)"
    else
        print_status "Neovim already installed: $(nvim --version | head -1)"
    fi
}

# Function to install Oh My Zsh
install_oh_my_zsh() {
    if [ ! -d "$HOME/.oh-my-zsh" ]; then
        print_status "Installing Oh My Zsh..."
        
        # Install Oh My Zsh
        sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" "" --unattended
        
        print_success "Oh My Zsh installed"
    else
        print_status "Oh My Zsh already installed"
    fi
}

# Function to install Oh My Posh
install_oh_my_posh() {
    if ! command_exists oh-my-posh; then
        print_status "Installing Oh My Posh..."
        
        # Download Oh My Posh
        curl -s https://ohmyposh.dev/install.sh | bash -s
        
        # Add to PATH
        echo 'export PATH=$PATH:$HOME/.local/bin' >> ~/.bashrc
        echo 'export PATH=$PATH:$HOME/.local/bin' >> ~/.zshrc
        
        print_success "Oh My Posh installed"
    else
        print_status "Oh My Posh already installed: $(oh-my-posh --version)"
    fi
}

# Function to install Chezmoi
install_chezmoi() {
    if ! command_exists chezmoi; then
        print_status "Installing Chezmoi..."
        
        # Install Chezmoi
        sh -c "$(curl -fsLS get.chezmoi.io)" -- init --apply $USER
        
        print_success "Chezmoi installed: $(chezmoi --version)"
    else
        print_status "Chezmoi already installed: $(chezmoi --version)"
    fi
}

# Function to install additional tools
install_additional_tools() {
    print_status "Installing additional tools..."
    
    # Install additional utilities
    apt-get install -y \
        tldr \
        tig \
        lazygit \
        delta \
        bottom \
        procs \
        sd \
        tealdeer \
        bandwhich \
        tokei \
        hyperfine \
        criterion-dev
    
    print_success "Additional tools installed"
}

# Function to configure Zsh
configure_zsh() {
    print_status "Configuring Zsh..."
    
    # Set Zsh as default shell
    chsh -s $(which zsh)
    
    # Create .zshrc if it doesn't exist
    if [ ! -f "$HOME/.zshrc" ]; then
        cat > "$HOME/.zshrc" << 'EOF'
# Oh My Zsh configuration
export ZSH="$HOME/.oh-my-zsh"
ZSH_THEME="robbyrussell"
plugins=(git docker docker-compose npm node rust go python pip tmux)
source $ZSH/oh-my-zsh.sh

# Oh My Posh configuration
eval "$(oh-my-posh init zsh --config ~/.poshthemes/atomic.omp.json)"

# Custom aliases
alias ll='exa -la'
alias tree='exa --tree'
alias cat='bat'
alias grep='rg'
alias find='fd'
alias top='btm'
alias ps='procs'

# Development aliases
alias gs='git status'
alias ga='git add'
alias gc='git commit'
alias gp='git push'
alias gl='git log --oneline --graph'

# Haskell/Stack aliases
alias sb='stack build'
alias sr='stack run'
alias st='stack test'
alias si='stack install'

# Docker aliases
alias d='docker'
alias dc='docker-compose'
alias dps='docker ps'
alias dimg='docker images'

# Path additions
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="/usr/local/go/bin:$PATH"

# Environment variables
export EDITOR=nvim
export VISUAL=nvim
export PAGER=less

# History configuration
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.zsh_history
setopt SHARE_HISTORY
setopt EXTENDED_HISTORY
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE

# Completion
autoload -U compinit
compinit

# Key bindings
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down
EOF
    fi
    
    print_success "Zsh configured"
}

# Function to configure Neovim
configure_neovim() {
    print_status "Configuring Neovim..."
    
    # Create Neovim config directory
    mkdir -p "$HOME/.config/nvim"
    
    # Create basic init.vim
    cat > "$HOME/.config/nvim/init.vim" << 'EOF'
" Basic Neovim configuration
set number
set relativenumber
set autoindent
set smartindent
set expandtab
set tabstop=4
set shiftwidth=4
set softtabstop=4
set smarttab
set ignorecase
set smartcase
set incsearch
set hlsearch
set showmatch
set wildmenu
set wildmode=list:longest
set ruler
set showcmd
set showmode
set laststatus=2
set statusline=%F%m%r%h%w\ [FORMAT=%{&ff}]\ [TYPE=%Y]\ [POS=%l,%v][%p%%]\ %{strftime(\"%d/%m/%y\ -\ %H:%M\")}
set backspace=indent,eol,start
set history=1000
set undolevels=1000
set title
set visualbell
set noerrorbells
set nobackup
set noswapfile
set viminfo='1000
set wildignore=*.swp,*.bak,*.pyc,*.class
set scrolloff=3
set sidescrolloff=5
set mouse=a
set clipboard=unnamedplus

" File type specific settings
filetype plugin indent on
syntax on

" Color scheme
colorscheme desert

" Key mappings
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Plugin manager (vim-plug)
call plug#begin('~/.local/share/nvim/plugged')

" Essential plugins
Plug 'neovim/nvim-lspconfig'
Plug 'hrsh7th/nvim-cmp'
Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'hrsh7th/cmp-buffer'
Plug 'hrsh7th/cmp-path'
Plug 'hrsh7th/cmp-cmdline'
Plug 'L3MON4D3/LuaSnip'
Plug 'saadparwaiz1/cmp_luasnip'
Plug 'nvim-treesitter/nvim-treesitter'
Plug 'nvim-telescope/telescope.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'kyazdani42/nvim-tree.lua'
Plug 'akinsho/bufferline.nvim'
Plug 'nvim-lualine/lualine.nvim'
Plug 'lewis6991/gitsigns.nvim'
Plug 'numToStr/Comment.nvim'
Plug 'windwp/nvim-autopairs'
Plug 'windwp/nvim-ts-autotag'

" Language specific
Plug 'neovimhaskell/haskell-vim'
Plug 'rust-lang/rust.vim'
Plug 'fatih/vim-go'
Plug 'python-mode/python-mode'
Plug 'dense-analysis/ale'

call plug#end()

" LSP configuration
lua << EOF
require'lspconfig'.hls.setup{}
require'lspconfig'.rust_analyzer.setup{}
require'lspconfig'.gopls.setup{}
require'lspconfig'.pyright.setup{}
EOF
EOF
    
    print_success "Neovim configured"
}

# Function to configure Tmux
configure_tmux() {
    print_status "Configuring Tmux..."
    
    # Create .tmux.conf
    cat > "$HOME/.tmux.conf" << 'EOF'
# Tmux configuration
set -g default-terminal "screen-256color"
set -g history-limit 10000
set -g base-index 1
setw -g pane-base-index 1

# Enable mouse
set -g mouse on

# Split panes using | and -
bind | split-window -h
bind - split-window -v
unbind '"'
unbind %

# Reload config file
bind r source-file ~/.tmux.conf \; display "Config reloaded!"

# Switch panes using Alt-arrow without prefix
bind -n M-Left select-pane -L
bind -n M-Right select-pane -R
bind -n M-Up select-pane -U
bind -n M-Down select-pane -D

# Switch windows using Alt-arrow without prefix
bind -n M-h previous-window
bind -n M-l next-window

# Resize panes using Alt-arrow without prefix
bind -n M-j resize-pane -D 5
bind -n M-k resize-pane -U 5
bind -n M-h resize-pane -L 5
bind -n M-l resize-pane -R 5

# Status bar
set -g status-style bg=black,fg=white
set -g window-status-current-style bg=white,fg=black,bold
set -g status-left-length 60
set -g status-right-length 60
set -g status-left '#[fg=green]#H #[fg=black]• #[fg=green]#(uname -r | cut -c 1-6)#[default]'
set -g status-right '#[fg=black]• #[fg=green]%Y-%m-%d #[fg=white]%H:%M#[default]'

# Copy mode
setw -g mode-keys vi
bind-key -T copy-mode-vi v send-keys -X begin-selection
bind-key -T copy-mode-vi y send-keys -X copy-selection
unbind -T copy-mode-vi Enter
bind-key -T copy-mode-vi Enter send-keys -X copy-pipe-and-cancel "xclip -selection clipboard"
EOF
    
    print_success "Tmux configured"
}

# Function to install and configure Haskell Stack
install_haskell_stack() {
    if ! command_exists stack; then
        print_status "Installing Haskell Stack..."
        
        # Install Stack
        curl -sSL https://get.haskellstack.org/ | sh
        
        # Add to PATH
        echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.bashrc
        echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.zshrc
        
        print_success "Haskell Stack installed: $(stack --version)"
    else
        print_status "Haskell Stack already installed: $(stack --version)"
    fi
}

# Function to create development directories
create_dev_directories() {
    print_status "Creating development directories..."
    
    # Create common development directories
    mkdir -p ~/projects
    mkdir -p ~/scripts
    mkdir -p ~/tools
    mkdir -p ~/downloads
    mkdir -p ~/backups
    
    print_success "Development directories created"
}

# Function to install additional development tools
install_dev_tools() {
    print_status "Installing additional development tools..."
    
    # Install additional packages
    apt-get install -y \
        clang \
        clang-tidy \
        cmake \
        ninja-build \
        valgrind \
        gdb \
        strace \
        ltrace \
        perf \
        flamegraph
    
    print_success "Development tools installed"
}

# Function to configure firewall
configure_firewall() {
    print_status "Configuring firewall..."
    
    # Install ufw if not present
    if ! command_exists ufw; then
        apt-get install -y ufw
    fi
    
    # Configure basic firewall rules
    ufw --force enable
    ufw default deny incoming
    ufw default allow outgoing
    ufw allow ssh
    ufw allow 80/tcp
    ufw allow 443/tcp
    
    print_success "Firewall configured"
}

# Function to show final summary
show_final_summary() {
    print_success "🎉 Server configuration completed!"
    echo
    echo "📋 What's been installed and configured:"
    echo "   ✅ System packages updated"
    echo "   ✅ Essential development tools"
    echo "   ✅ Node.js, npm, yarn, pnpm"
    echo "   ✅ Rust and Cargo"
    echo "   ✅ Go programming language"
    echo "   ✅ Python tools and packages"
    echo "   ✅ Docker and Docker Compose"
    echo "   ✅ Neovim with configuration"
    echo "   ✅ Oh My Zsh with Oh My Posh"
    echo "   ✅ Chezmoi for dotfile management"
    echo "   ✅ Tmux with configuration"
    echo "   ✅ Haskell Stack"
    echo "   ✅ Additional development tools"
    echo "   ✅ Firewall configuration"
    echo
    echo "🔧 Next steps:"
    echo "   1. Restart your shell: exec zsh"
    echo "   2. Customize your dotfiles with Chezmoi"
    echo "   3. Install additional Neovim plugins"
    echo "   4. Configure your development environment"
    echo
    echo "📊 Installed versions:"
    echo "   Node.js: $(node --version 2>/dev/null || echo 'Not available')"
    echo "   Rust: $(cargo --version 2>/dev/null || echo 'Not available')"
    echo "   Go: $(go version 2>/dev/null || echo 'Not available')"
    echo "   Neovim: $(nvim --version | head -1 2>/dev/null || echo 'Not available')"
    echo "   Stack: $(stack --version 2>/dev/null || echo 'Not available')"
    echo
    echo "🚀 Your server is now ready for development!"
}

# Main execution
main() {
    echo "🚀 Starting PSYOP server configuration..."
    echo "   This will install and configure all development tools"
    echo "   Estimated time: 10-15 minutes"
    echo
    
    # Check if running as root
    if [ "$EUID" -eq 0 ]; then
        print_error "Please don't run this script as root"
        print_error "Run as a regular user with sudo privileges"
        exit 1
    fi
    
    # Update system
    update_system
    
    # Install packages
    install_essential_packages
    install_nodejs
    install_rust
    install_go
    install_python_tools
    install_docker
    install_neovim
    install_oh_my_zsh
    install_oh_my_posh
    install_chezmoi
    install_additional_tools
    install_haskell_stack
    install_dev_tools
    
    # Configure tools
    configure_zsh
    configure_neovim
    configure_tmux
    
    # Create directories
    create_dev_directories
    
    # Configure firewall
    configure_firewall
    
    # Show summary
    show_final_summary
}

# Run main function
main "$@"
