$a = [Windows.Networking.NetworkOperators.NetworkOperatorTetheringManager, Windows.Networking.NetworkOperators, ContentType=WindowsRuntime]::CreateFromConnectionProfile(
         [Windows.Networking.Connectivity.NetworkInformation, Windows.Networking.Connectivity, ContentType=WindowsRuntime]::GetInternetConnectionProfile()
     )
$a.StartTetheringAsync() # to start the Mobile hotspot
# $a.StopTetheringAsync()  # to stop the Mobile hotspot
