 # Param must be the first statement in your script
param(
    [Parameter(Position = 0, Mandatory = $true)]
    [String]$DestUserDomain=$env:USERDOMAIN
    ,
    [Parameter(Position = 1, Mandatory = $true)]
    [String]$DestUserName=$env:USERNAME
    ,
    [Parameter(Position = 2, Mandatory = $true)]
    [String]$DestHome=$HOME
    )

$OS = "windows"
$GIT_USER_CONFIG_FILE = "$DestHome\.gitconfig_user"
$HG_USER_CONFIG_FILE = "$DestHome\mercurial_user.ini"

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

Function Install-Registry {
    $registryFiles = Get-ChildItem $DOTFILES\*\* | Where-Object {$_.Name -match "\.($OS-)?registry$"}
    $sid = Get-SidFromUser -DomainName $DestUserDomain -UserName $DestUserName
    $hkcu = "HKCU:"
    foreach ($file in $registryFiles) {
        $fileContent = Get-Content -LiteralPath $file | Out-String
        $jsonRegistry = ConvertFrom-Json -InputObject $fileContent

        foreach ($registryEntry in $jsonRegistry.RegistryEntries) {
            # Se in json ho un path che deve lavorare su HKCU non posso
            # usare direttamente HKCU perche' prenderebbe quello impersonato
            # (l'utente administrator).
            # Devo quindi sostituirlo con HKU riferito al sid dell'utente
            # che ha effettivamente lanciato lo script tramite il batch di avvio
            if ($registryEntry.Path.StartsWith($hkcu)) {
                $newString = "HKU:\$sid"
                $registryPath = $registryEntry.Path -replace $hkcu, $newString
            } else {
                $registryPath = $registryEntry.Path
            }
            # Verifico che il percorso nel registro esista, altrimenti lo creo
            if (-not (Test-Path -LiteralPath $registryPath)) {
                New-Item -Path $registryPath -Force
            }
            # Verifico l'esistenza della chiave
            if (Test-RegistryValue -Path $registryPath -Name $registryEntry.Key.Name) {
                # La chiave esiste. La elimino
                Remove-ItemProperty -LiteralPath $registryPath -Name $registryEntry.Key.Name
            }
            # Nel file Json sono ammessi i seguenti Type:
            # String. Specifies a null-terminated string. Equivalent to REG_SZ.
            # ExpandString. Specifies a null-terminated string that contains unexpanded references to environment variables that are expanded when the value is retrieved. Equivalent to REG_EXPAND_SZ.
            # Binary. Specifies binary data in any form. Equivalent to REG_BINARY.
            # DWord. Specifies a 32-bit binary number. Equivalent to REG_DWORD.
            # MultiString. Specifies an array of null-terminated strings terminated by two null characters. Equivalent to REG_MULTI_SZ.
            # Qword. Specifies a 64-bit binary number. Equivalent to REG_QWORD.
            # Unknown. Indicates an unsupported registry data type, such as REG_RESOURCE_LIST.
            New-ItemProperty -LiteralPath $registryPath -Name $registryEntry.Key.Name -PropertyType $registryEntry.Key.Type -Value $registryEntry.Key.Value
        }
    }
}

Function Install-Symlinks {
    $filesToSymlink = Get-ChildItem $DOTFILES\*\* | Where-Object {$_.Name -match "\.($OS-)?symlink$"}

    foreach ($file in $filesToSymlink) {
        $name = Get-Basename $file.Name
        $symlink = "$DestHome\$name"
        $target = $file.FullName

        New-Symlink "$symlink" "$target"
    }
}

Function Install-ConfigurableSymlinks {
    $symlinkConfigFiles = Get-ChildItem $DOTFILES\*\* | Where-Object {$_.Name -match "\.symlinks$"}

    foreach ($configFile in $symlinkConfigFiles) {
        $config = Get-Content $configFile.FullName
		$matches = $null
        $configuredName = $config | ForEach-Object {$null = $_ -match "^$OS" + ':\s+(?<link>.+)$'; $matches.link} | Select-Object -f 1

        if ($configuredName) {
            $symlink = "$DestHome\$configuredName"
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

Function Get-SidFromUser {
    param (
        [Parameter(Position = 0, Mandatory = $true, ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true)]
        [String]$DomainName
        ,
        [Parameter(Position = 1, Mandatory = $true, ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true)]
        [String]$UserName
    )

    process {
        $objUser = New-Object System.Security.Principal.NTAccount($DomainName, $UserName)
        $strSID = $objUser.Translate([System.Security.Principal.SecurityIdentifier])
        $strSID.Value
    }
}

Function Test-RegistryValue {
    param(
        [Alias("PSPath")]
        [Parameter(Position = 0, Mandatory = $true, ValueFromPipeline = $true, ValueFromPipelineByPropertyName = $true)]
        [String]$Path
        ,
        [Parameter(Position = 1, Mandatory = $true)]
        [String]$Name
        ,
        [Switch]$PassThru
    ) 

    process {
        if (Test-Path $Path) {
            $Key = Get-Item -LiteralPath $Path
            if ($Key.GetValue($Name, $null) -ne $null) {
                if ($PassThru) {
                    Get-ItemProperty $Path $Name
                } else {
                    $true
                }
            } else {
                $false
            }
        } else {
            $false
        }
    }
 }

New-PSDrive -PSProvider Registry -Name HKU -Root HKEY_USERS

Write-Host "-> Dotfiles directory = $DOTFILES"
Write-Host "-> Configuring Git and Mercurial user..."
Set-DvcsUser
Write-Host "-> Creating registry data..."
Install-Registry
Write-Host "-> Creating symbolic links..."
Install-Symlinks
Install-ConfigurableSymlinks
Write-Host "-> Done!"
Write-Host -NoNewLine "Press any key to continue..."

$null = $Host.UI.RawUI.ReadKey("NoEcho,IncludeKeyDown")