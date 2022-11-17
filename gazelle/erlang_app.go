package erlang

type erlangApp struct {
	Name         string
	Description  string
	Version      string
	Srcs         MutableSet[string]
	PrivateHdrs  MutableSet[string]
	PublicHdrs   MutableSet[string]
	AppSrc       MutableSet[string]
	TestSrcs     MutableSet[string]
	TestHdrs     MutableSet[string]
	LicenseFiles MutableSet[string]
	ErlcOpts     []string
	TestErlcOpts []string
	Deps         MutableSet[string]
	ExtraApps    MutableSet[string]
}

func newErlangApp() *erlangApp {
	return &erlangApp{
		Srcs:         NewMutableSet[string](),
		PrivateHdrs:  NewMutableSet[string](),
		PublicHdrs:   NewMutableSet[string](),
		AppSrc:       NewMutableSet[string](),
		TestSrcs:     NewMutableSet[string](),
		TestHdrs:     NewMutableSet[string](),
		LicenseFiles: NewMutableSet[string](),
		ErlcOpts:     []string{"+debug_info"},
		TestErlcOpts: []string{"+debug_info", "-DTEST=1"},
		Deps:         NewMutableSet[string](),
		ExtraApps:    NewMutableSet[string](),
	}
}
