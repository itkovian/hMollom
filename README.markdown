# hMollom

This software bundle contains hMollom, a Haskell library for interfacing with the
Mollom anti-spam service. For more information about Mollom, see http://mollom.com.

## Release versions

- 0.3.1  Minor fix to the exported functions and data types from the Network.Mollom
         module. If you need anything besides runMollom, you should import the other
         modules in a qualified form, since severel data types have been given the
         same name.
- 0.3.0  Moved hMollom to target the new [REST API][id]. All replies are requested in JSON
         format and are presented to the user in the Mollom monad as part of a 
         MollomResponse data type. The response body is provided as a record with
         the fields corresponding to the keys in the JSON data structure. This version
         is not present on Hackage.
- 0.2.2  Final XML-RPC version.
- 0.2    Using a monadic approach to capture state changes across different calls to
         the Mollom service.
- 0.1.1  Bugfix release.
- 0.1    Initial release.




[rest api]: http://mollom.com/api/rest
