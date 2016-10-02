param([String]$destHome=$HOME) #Must be the first statement in your script

$OS = "windows"
$GIT_USER_CONFIG_FILE = "$destHome\.gitconfig_user"
$HG_USER_CONFIG_FILE = "$destHome\mercurial_user.ini"

$scriptRoot = Split-Path -Path $MyInvocation.MyCommand.Path
#$DOTFILES = Get-Location
$DOTFILES = $scriptRoot

Function Set-DvcsUser {
    if ((Test-Path $GIT_USER_CONFIG_FILE) -And (Test-Path $HG_USER_CONFIG_FILE)) {
        Write-Host "-> GIT user config file $GIT_USER_CONFIG_FILE already exists. Skipping..."
		Write-Host "-> HG user config file $HG_USER_CONFIG_FILE already exists. Skipping..."
    }
    else {
		if (!(Test-Path $GIT_USER_CONFIG_FILE) -And !(Test-Path $HG_USER_CONFIG_FILE)) {
			Write-Host "-> Asking Username & Email for HG and GIT user config files (${HG_USER_CONFIG_FILE}, ${GIT_USER_CONFIG_FILE}):"
		}
		else {
			if (!(Test-Path $GIT_USER_CONFIG_FILE)) {
				Write-Host "-> Asking Username & Email for GIT user config file ${GIT_USER_CONFIG_FILE}:"
			}
            if (!(Test-Path $HG_USER_CONFIG_FILE)) {
				Write-Host "-> Asking Username & Email for HG user config file ${HG_USER_CONFIG_FILE}:"
			}
		}
        $userName = Read-Host "Name"
        $userEmail = Read-Host "Email"
		if (!(Test-Path $GIT_USER_CONFIG_FILE)) {
			Write-Host "-> Creating ${GIT_USER_CONFIG_FILE}:"
			@"
[user]
    name = $userName
    email = $userEmail
"@ | Set-Content $GIT_USER_CONFIG_FILE

			$answer = Read-Host "`nDo you want to add an organization-specific email? [y/N]"

			if ($answer -match '^[yY]$') {
				$orgName = Read-Host "Organization"
				$orgUserEmail = Read-Host "Organization Email"

				@"
[orgs "$orgName"]
    email = $orgUserEmail
"@ | Add-Content $GIT_USER_CONFIG_FILE
			}

			Write-Host "`n$GIT_USER_CONFIG_FILE created!"
		}
		if (!(Test-Path $HG_USER_CONFIG_FILE)) {
			Write-Host "-> Creating ${HG_USER_CONFIG_FILE}:"
			@"
[ui]
username = $userName <$userEmail>
"@ | Set-Content $HG_USER_CONFIG_FILE
			Write-Host "`n$HG_USER_CONFIG_FILE created!"
		}
    }
}

Function Install-Symlinks {
    $filesToSymlink = Get-ChildItem $DOTFILES\*\* | where {$_.Name -match "\.($OS-)?symlink$"}

    foreach ($file in $filesToSymlink) {
        $name = Get-Basename $file.Name
        $symlink = "$destHome\$name"
        $target = $file.FullName

        New-Symlink "$symlink" "$target"
    }
}

Function Install-ConfigurableSymlinks {
    $symlinkConfigFiles = Get-ChildItem $DOTFILES\*\* | where {$_.Name -match "\.symlinks$"}

    foreach ($configFile in $symlinkConfigFiles) {
        $config = cat $configFile.FullName
		$matches = $null
        $configuredName = $config | % {$null = $_ -match "^$OS" + ':\s+(?<link>.+)$'; $matches.link} | select -f 1

        if ($configuredName) {
            $symlink = "$destHome\$configuredName"
            $target = Get-Basename $configFile.FullName

            New-Symlink "$symlink" "$target"
        }
    }
}

Function Get-Basename {
    Param($string)
    return $string.Substring(0, $string.LastIndexOf('.'))
}

Function New-Symlink {
    Param($symlink, $target)

    if (Test-Path $target -pathType container) {
        # Remove-Item cannot be used to remove folder symlinks,
        # because it also removes the target folder.
        if (Test-Path $symlink) { cmd /c rmdir /s /q $symlink }
        (cmd /c mklink /d $symlink $target) > $null
    }
    else {
        if (Test-Path $symlink) { Remove-Item $symlink }
        (cmd /c mklink $symlink $target) > $null
    }

    Write-Host "   $symlink -> $target"
}

Write-Host "-> Dotfiles directory = $DOTFILES"
Write-Host "-> Configuring Git and Mercurial user..."
Set-DvcsUser
Write-Host "-> Creating symbolic links..."
Install-Symlinks
Install-ConfigurableSymlinks
Write-Host "-> Done!"
Write-Host -NoNewLine "Press any key to continue..."

$null = $Host.UI.RawUI.ReadKey("NoEcho,IncludeKeyDown")