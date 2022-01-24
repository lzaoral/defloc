%global pkg_name defloc

%bcond_without tests

Name:           %{pkg_name}
Version:        0.2.0.0
Release:        1%{?dist}
Summary:        Finds locations of function definitions in shell scripts

License:        GPLv3+
Url:            https://github.com/lzaoral/%{name}
Source0:        %{url}/archive/refs/tags/v%{version}/%{name}-%{version}.tar.gz

BuildRequires:  ghc-Cabal-devel
BuildRequires:  ghc-rpm-macros
BuildRequires:  ghc-ShellCheck-prof
BuildRequires:  ghc-base-prof
BuildRequires:  ghc-containers-prof
BuildRequires:  ghc-regex-pcre-prof
%if %{with tests}
BuildRequires:  ghc-HUnit-devel
BuildRequires:  ghc-filepath-devel
%endif

%description
Simple tool based on ShellCheck's parser that can be used to find definitions
of functions in shell scripts.


%package -n ghc-%{name}
Summary:        Haskell %{name} library

%description -n ghc-%{name}
This package contains the Haskell %{name} library.


%package -n ghc-%{name}-devel
Summary:        Haskell %{pkg_name} library development files
Provides:       ghc-%{name}-static = %{version}-%{release}
Provides:       ghc-%{name}-static%{?_isa} = %{version}-%{release}
%if %{defined ghc_version}
Requires:       ghc-compiler = %{ghc_version}
%endif
Requires:       ghc-%{name}%{?_isa} = %{version}-%{release}

%description -n ghc-%{name}-devel
This package provides the Haskell %{pkg_name} library development files.


%if %{with haddock}
%package -n ghc-%{name}-doc
Summary:        Haskell %{name} library documentation
BuildArch:      noarch
Requires:       ghc-filesystem

%description -n ghc-%{name}-doc
This package provides the Haskell %{name} library documentation.
%endif


%if %{with ghc_prof}
%package -n ghc-%{name}-prof
Summary:        Haskell %{name} profiling library
Requires:       ghc-%{name}-devel%{?_isa} = %{version}-%{release}
Supplements:    (ghc-%{name}-devel and ghc-prof)

%description -n ghc-%{name}-prof
This package provides the Haskell %{name} profiling library.
%endif


%prep
%setup -q


%build
%ghc_lib_build


%install
%ghc_lib_install


%check
%if %{with tests}
%cabal_test
%endif


%files
%license LICENSE
%doc CHANGELOG.md README.md
%{_bindir}/%{name}


%files -n ghc-%{name} -f ghc-%{name}.files
%license LICENSE


%files -n ghc-%{name}-devel -f ghc-%{name}-devel.files


%if %{with haddock}
%files -n ghc-%{name}-doc -f ghc-%{name}-doc.files
%license LICENSE
%endif


%if %{with ghc_prof}
%files -n ghc-%{name}-prof -f ghc-%{name}-prof.files
%endif


%changelog
* Mon Jan 24 2022 Lukáš Zaoral <lzaoral@redhat.com> - 0.2.0.0-1
- Try to process files that contain errors as well.
- Remove monad transformers from the code base.
- Add a simple test-suite.

* Tue Dec  7 2021 Lukáš Zaoral <lzaoral@redhat.com> - 0.1.0.0-1
- Initial release
