/******************************************************************************
 *            Copyright (c) 2006-2009 Michel Gutierrez. All Rights Reserved.
 ******************************************************************************/

/**
 * Constants.
 */

const NS_DUMPPROC_CID = Components.ID("{c0b558fd-d32a-4b7f-ae48-5ef095134292}");
const NS_DUMPPROC_PROG_ID = "@downloadhelper.net/dump-processor;1";
const DHNS = "http://downloadhelper.net/1.0#";

var Util=null;

/**
* Object constructor
*/
function DumpProc() {
	try {
		//dump("[DumpProc] constructor\n");

		var prefService=Components.classes["@mozilla.org/preferences-service;1"]
		                                   .getService(Components.interfaces.nsIPrefService);
		this.pref=prefService.getBranch("dwhelper.");
		var dpe=false;
		try {
			dpe=this.pref.getBoolPref("dump-processor-enable");
		} catch(e) {}
		if(dpe) {
			this.core=Components.classes["@downloadhelper.net/core;1"].
				getService(Components.interfaces.dhICore);
			this.core.registerProcessor(this);
		}
	} catch(e) {
		dump("[DumpProc] !!! constructor: "+e+"\n");
	}
}

DumpProc.prototype = {
		get name() { return "dump"; },
		get provider() { return "DownloadHelper"; },
		get title() { return Util.getText("processor.dump.title"); },
		get description() { return Util.getText("processor.dump.description"); },
		get enabled() { return true; },
}

DumpProc.prototype.canHandle=function(desc) {
	//dump("[DumpProc] canHandle()\n");
	return true;
}

DumpProc.prototype.requireDownload=function(desc) {
	//dump("[DumpProc] requireDownload()\n");
	return false;
}

DumpProc.prototype.preDownload=function(desc) {
	//dump("[DumpProc] preDownload()\n");
	return true;
}

DumpProc.prototype.handle=function(desc) {
	//dump("[DumpProc] handle()\n");
    var wm = Components.classes["@mozilla.org/appshell/window-mediator;1"]
                                .getService(Components.interfaces.nsIWindowMediator);
	var w = wm.getMostRecentWindow("navigator:browser");
	w.openDialog('chrome://dwhelper/content/dump-media.xul','_blank',"chrome,centerscreen",desc);
}

DumpProc.prototype.QueryInterface = function(iid) {
	//dump("[DumpProc] QueryInterface("+iid+")\n");
    if(
    	iid.equals(Components.interfaces.dhIProcessor) ||
    	iid.equals(Components.interfaces.nsISupports)
	) {
	    return this;
    }
	throw Components.results.NS_ERROR_NO_INTERFACE;
}

var vDumpProcModule = {
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
        compMgr.registerFactoryLocation(NS_DUMPPROC_CID,
                                        "DumpProc",
                                        NS_DUMPPROC_PROG_ID, 
                                        fileSpec,
                                        location,
                                        type);
    },

	unregisterSelf: function(compMgr, fileSpec, location) {
    	compMgr = compMgr.QueryInterface(Components.interfaces.nsIComponentRegistrar);
    	compMgr.unregisterFactoryLocation(NS_DH_DUMPPROC_CID, fileSpec);
	},

    /*
     * The GetClassObject method is responsible for producing Factory and
     * SingletonFactory objects (the latter are specialized for services).
     */
    getClassObject: function (compMgr, cid, iid) {
        if (!cid.equals(NS_DUMPPROC_CID)) {
	    	throw Components.results.NS_ERROR_NO_INTERFACE;
		}

        if (!iid.equals(Components.interfaces.nsIFactory)) {
	    	throw Components.results.NS_ERROR_NOT_IMPLEMENTED;
		}

        return this.vDumpProcFactory;
    },

    /* factory object */
    vDumpProcFactory: {
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

			return new DumpProc().QueryInterface(iid);
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
    return vDumpProcModule;
}

