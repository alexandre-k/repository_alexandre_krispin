/******************************************************************************
 *            Copyright (c) 2006-2010 Michel Gutierrez. All Rights Reserved.
 ******************************************************************************/

/**
 * Constants.
 */

const NS_SEHPROC_CID = Components.ID("{3b6dfafc-a55a-4e3b-8e2c-34584c33d676}");
const NS_SEHPROC_PROG_ID = "@downloadhelper.net/secrethelper-intro-processor;1";
const DHNS = "http://downloadhelper.net/1.0#";

var Util=null;

/**
* Object constructor
*/
function SehProc() {
	try {
		//dump("[SehProc] constructor\n");

		var prefService=Components.classes["@mozilla.org/preferences-service;1"]
		                                   .getService(Components.interfaces.nsIPrefService);
		this.pref=prefService.getBranch("dwhelper.");
		var seh=this.pref.getBoolPref("seh-intro-proc.enable");
		if(seh) {	
			try { // for windows only
				var reg=Components.classes["@mozilla.org/windows-registry-key;1"]
					.createInstance(Components.interfaces.nsIWindowsRegKey);
				this.core=Components.classes["@downloadhelper.net/core;1"].
					getService(Components.interfaces.dhICore);
				this.core.registerProcessor(this);
			} catch(e) {}
		}
	} catch(e) {
		dump("[SehProc] !!! constructor: "+e+"\n");
	}
}

SehProc.prototype = {
		get name() { return "sec-download"; },
		get provider() { return "SecretHelperHelper"; },
		get title() { return Util.getText("processor.sec-download.title"); },
		get description() { return Util.getText("processor.sec-download.description"); },
		get enabled() { return true; },
}

SehProc.prototype.canHandle=function(desc) {
	//dump("[SehProc] canHandle()\n");
	if(desc.has("media-url"))
		return true;
	else
		return false;
}

SehProc.prototype.requireDownload=function(desc) {
	//dump("[SehProc] requireDownload()\n");
	return false;
}

SehProc.prototype.preDownload=function(desc) {
	//dump("[SehProc] preDownload()\n");
	return true;
}

SehProc.prototype.handle=function(desc) {
	//dump("[SehProc] handle()\n");
    var wm = Components.classes["@mozilla.org/appshell/window-mediator;1"]
                                .getService(Components.interfaces.nsIWindowMediator);
	var w = wm.getMostRecentWindow("navigator:browser");
	w.open('http://www.downloadhelper.net/secrethelper.php');
}

SehProc.prototype.QueryInterface = function(iid) {
	//dump("[SehProc] QueryInterface("+iid+")\n");
    if(
    	iid.equals(Components.interfaces.dhIProcessor) ||
    	iid.equals(Components.interfaces.nsISupports)
	) {
	    return this;
    }
	throw Components.results.NS_ERROR_NO_INTERFACE;
}

var vSehProcModule = {
    firstTime: true,
    
    /*
     * RegisterSelf is called at registration time (component installation
     * or the only-until-release startup autoregistration) and is responsible
     * for notifying the component manager of all components implemented in
     * this module.  The fileSpec, location and type parameters are mostly
     * opaque, and should be passed on to the registerComponent call
     * unmolested.
     */
    registerSelf: function (compMgr, fileSpec, location, type) {

        if (this.firstTime) {
            this.firstTime = false;
            throw Components.results.NS_ERROR_FACTORY_REGISTER_AGAIN;
        }
        compMgr = compMgr.QueryInterface(Components.interfaces.nsIComponentRegistrar);
        compMgr.registerFactoryLocation(NS_SEHPROC_CID,
                                        "SehProc",
                                        NS_SEHPROC_PROG_ID, 
                                        fileSpec,
                                        location,
                                        type);
    },

	unregisterSelf: function(compMgr, fileSpec, location) {
    	compMgr = compMgr.QueryInterface(Components.interfaces.nsIComponentRegistrar);
    	compMgr.unregisterFactoryLocation(NS_DH_SEHPROC_CID, fileSpec);
	},

    /*
     * The GetClassObject method is responsible for producing Factory and
     * SingletonFactory objects (the latter are specialized for services).
     */
    getClassObject: function (compMgr, cid, iid) {
        if (!cid.equals(NS_SEHPROC_CID)) {
	    	throw Components.results.NS_ERROR_NO_INTERFACE;
		}

        if (!iid.equals(Components.interfaces.nsIFactory)) {
	    	throw Components.results.NS_ERROR_NOT_IMPLEMENTED;
		}

        return this.vSehProcFactory;
    },

    /* factory object */
    vSehProcFactory: {
        /*
         * Construct an instance of the interface specified by iid, possibly
         * aggregating it with the provided outer.  (If you don't know what
         * aggregation is all about, you don't need to.  It reduces even the
         * mightiest of XPCOM warriors to snivelling cowards.)
         */
        createInstance: function (outer, iid) {
            if (outer != null) {
				throw Components.results.NS_ERROR_NO_AGGREGATION;
	    	}
	
	    	if(Util==null) 
	    		Util=Components.classes["@downloadhelper.net/util-service;1"]
					.getService(Components.interfaces.dhIUtilService);

			return new SehProc().QueryInterface(iid);
        }
    },

    /*
     * The canUnload method signals that the component is about to be unloaded.
     * C++ components can return false to indicate that they don't wish to be
     * unloaded, but the return value from JS components' canUnload is ignored:
     * mark-and-sweep will keep everything around until it's no longer in use,
     * making unconditional ``unload'' safe.
     *
     * You still need to provide a (likely useless) canUnload method, though:
     * it's part of the nsIModule interface contract, and the JS loader _will_
     * call it.
     */
    canUnload: function(compMgr) {
		return true;
    }
};

function NSGetModule(compMgr, fileSpec) {
    return vSehProcModule;
}

