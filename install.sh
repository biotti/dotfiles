#!/usr/bin/env bash

# set -x

function ask() {
    # http://djm.me/ask
    local prompt default REPLY
    while true; do
        if [ "${2:-}" = "Y" ]; then
            prompt="Y/n"
            default=Y
        elif [ "${2:-}" = "N" ]; then
            prompt="y/N"
            default=N
        else
            prompt="y/n"
            default=
        fi
        # Ask the question (not using "read -p" as it uses stderr not stdout)
        echo -n "$1 [$prompt] "
        # Read the answer (use /dev/tty in case stdin is redirected from somewhere else)
        read REPLY </dev/tty
        # Default?
        if [ -z "$REPLY" ]; then
            REPLY=$default
        fi
        # Check if the reply is valid
        case "$REPLY" in
            Y*|y*) return 0 ;;
            N*|n*) return 1 ;;
        esac
    done
}

function set_dvcs_user() {
    local DVCS_USER_NAME
    local DVCS_USER_EMAIL
    local DVCS_ORG_NAME
    local DVCS_ORG_EMAIL
    if [[ -f $GIT_USER_CONFIG_FILE && -f $HG_USER_CONFIG_FILE ]] ; then
        echo "-> GIT user config file $GIT_USER_CONFIG_FILE already exists. Skipping..."
		echo "-> HG user config file $HG_USER_CONFIG_FILE already exists. Skipping..."
    else
        if [[ ! -f $GIT_USER_CONFIG_FILE && ! -f $HG_USER_CONFIG_FILE ]] ; then
            echo "-> Asking Username & Email for HG and GIT user config files ($HG_USER_CONFIG_FILE, $GIT_USER_CONFIG_FILE):"
        else
            if [[ ! -f $GIT_USER_CONFIG_FILE ]] ; then
                echo "-> Asking Username & Email for GIT user config file $GIT_USER_CONFIG_FILE:"
            fi
            if [[ ! -f $HG_USER_CONFIG_FILE ]] ; then
                echo "-> Asking Username & Email for HG user config file $HG_USER_CONFIG_FILE:"
            fi
        fi
        read -r -p "Name: " DVCS_USER_NAME
        read -r -p "Email: " DVCS_USER_EMAIL
        if [[ ! -f $GIT_USER_CONFIG_FILE ]] ; then
            echo "-> Creating $GIT_USER_CONFIG_FILE:"
            cat > $GIT_USER_CONFIG_FILE <<EOF
[user]
    name = $DVCS_USER_NAME
    email = $DVCS_USER_EMAIL
EOF
            if ask "Do you want to add an organization-specific email?" N; then
                read -r -p "Organization: " DVCS_ORG_NAME
                read -r -p "Organization Email: " DVCS_ORG_EMAIL
                cat >> $GIT_USER_CONFIG_FILE <<EOF
[orgs "$DVCS_ORG_NAME"]
    email = $DVCS_ORG_EMAIL
EOF
            fi
            echo "$GIT_USER_CONFIG_FILE created!"
        fi
        if [[ ! -f $HG_USER_CONFIG_FILE ]] ; then
            echo "-> Creating $HG_USER_CONFIG_FILE:"
            cat > $HG_USER_CONFIG_FILE <<EOF
[ui]
username = $DVCS_USER_NAME <$DVCS_USER_EMAIL>
EOF
        fi
    fi
}

function new_symlink() {
    local SYM_LINK
    local LINK_TARGET
    LINK_TARGET="$1"
    SYM_LINK="$2"
    if [[ ! -h $SYM_LINK ]]; then
        ln -s $LINK_TARGET $SYM_LINK
		echo "    $SYM_LINK -> $LINK_TARGET"
    else
		if [[ -h $SYM_LINK ]]; then
			rm $SYM_LINK
			ln -s $LINK_TARGET $SYM_LINK
			echo "    $SYM_LINK -> $LINK_TARGET"
		else
			echo "    Cannot link $LINK_TARGET to $SYM_LINK"
			echo "    because $SYM_LINK exists and is not a symbolic link"
		fi
    fi
}    

function get_basename() {
    local STRING_PARAM
    local RESULT
    eval "STRING_PARAM=\$$1"
    RESULT=${STRING_PARAM%.*}
	eval "$1=$RESULT"
}

function install_symlinks() {
    local LINK_FILE
    local LINK_FILE_NAME
    local SYM_LINK
    local LINK_TARGET
    for LINK_FILE in $(find $SCRIPT_DIR -name '*.symlink' -or -name '*.$OS_NAME-symlink'); do
        LINK_FILE_NAME=$(basename "$LINK_FILE")
        SYM_LINK="$DEST_DIR/$LINK_FILE_NAME"
		LINK_TARGET="$LINK_FILE"
		get_basename SYM_LINK
        new_symlink "$LINK_TARGET" "$SYM_LINK"
    done
}

function install_configurable_symlinks() {
    local LINK_FILE
    local CONFIGURED_NAME
    local SYM_LINK
    local LINK_TARGET
	local AWK_COMMAND
    for LINK_FILE in $(find $SCRIPT_DIR -name '*.symlinks'); do
		 AWK_COMMAND="awk 'match(\$0, /^$OS_NAME:\s+(.+)$/, mtch) {print mtch[1]}' $LINK_FILE"
		 CONFIGURED_NAME=$(eval $AWK_COMMAND)
         if [[ $CONFIGURED_NAME ]]; then
             SYM_LINK="$DEST_DIR/$CONFIGURED_NAME"
			 LINK_TARGET="$LINK_FILE"
             get_basename LINK_TARGET
             new_symlink "$LINK_TARGET" "$SYM_LINK"
         fi
    done
}    

DEST_DIR=$1

if [[ ! $DEST_DIR ]]; then
	DEST_DIR=$HOME
fi

OS_NAME="linux"
GIT_USER_CONFIG_FILE="$DEST_DIR/.gitconfig_user"
HG_USER_CONFIG_FILE="$DEST_DIR/.hgrc_user"

SCRIPT_SOURCE="${BASH_SOURCE[0]}"

# resolve $SCRIPT_SOURCE until the file is no longer a symlink
while [ -h "$SCRIPT_SOURCE" ]; do
    SCRIPT_DIR="$( cd -P "$( dirname "$SCRIPT_SOURCE" )" && pwd )"
    PVR_BASH_SOURCE="$(readlink "$SCRIPT_SOURCE")"
    [[ $SCRIPT_SOURCE != /* ]] && SCRIPT_SOURCE="$SCRIPT_DIR/$SCRIPT_SOURCE"
done

SCRIPT_DIR="$( cd -P "$( dirname "$SCRIPT_SOURCE" )" && pwd )"

if [[ ! -d $DEST_DIR ]]; then
	echo "Directory $DEST_DIR does not exists. Quitting"
	exit 1
else
	echo "-> Dotfiles directory = $SCRIPT_SOURCE"
	echo "-> Installing in $DEST_DIR"
	echo "-> Configuring Git and Mercurial user..."
	set_dvcs_user
	echo "-> Creating/updating symbolic links..."
	install_symlinks
	install_configurable_symlinks
	echo "-> Done!"
	read -r -p "Press any key to countinue... "
fi

# set +x
