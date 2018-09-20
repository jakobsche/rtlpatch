{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit rtlpatch;

interface

uses
  OP, Patch, DynArray, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('rtlpatch', @Register);
end.
