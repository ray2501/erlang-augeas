#
# spec file for package erlang-augeas
#

%define app_name augeas
Name:           erlang-%{app_name}
Version:        0.2.2
Release:        0
%define app_ver %(echo "%{version}" | cut -d "+" -f1)
Summary:        Erlang bindings for Augeas
License:        MIT
Group:          Development/Libraries/Other
Url:            https://github.com/ray2501/erlang-augeas
Source:         %{name}-%{version}.tar.gz
BuildRequires:  make
BuildRequires:  gcc
BuildRequires:  pkgconfig
BuildRequires:  pkgconfig(augeas)
BuildRequires:  erlang
Requires:       erlang
BuildRoot:      %{_tmppath}/%{name}-%{version}-build

%description
It is an Erlang bindings for Augeas.

%prep
%setup -q -n %{name}-%{version}

%build
make

%install
for dir in ebin priv ; do
	mkdir -p %{buildroot}%{erlang_libdir}/%{app_name}-%{app_ver}/${dir}
	cp -r ${dir}/* %{buildroot}%{erlang_libdir}/%{app_name}-%{app_ver}/${dir}/
done

%files
%defattr(-,root,root)
%doc README.md LICENSE
%dir %{erlang_libdir}/%{app_name}-%{app_ver}
%dir %{erlang_libdir}/%{app_name}-%{app_ver}/ebin
%{erlang_libdir}/%{app_name}-%{app_ver}/ebin/*.app
%{erlang_libdir}/%{app_name}-%{app_ver}/ebin/*.beam
%dir %{erlang_libdir}/%{app_name}-%{app_ver}/priv
%{erlang_libdir}/%{app_name}-%{app_ver}/priv/*.so

%changelog

