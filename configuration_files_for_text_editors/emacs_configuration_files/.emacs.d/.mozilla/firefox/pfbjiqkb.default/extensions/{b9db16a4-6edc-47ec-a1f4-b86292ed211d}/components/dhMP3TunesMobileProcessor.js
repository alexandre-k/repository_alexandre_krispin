/******************************************************************************
 *            Copyright (c) 2006-2009 Michel Gutierrez. All Rights Reserved.
 ******************************************************************************/

/**
 * Constants.
 */

const NS_MTMPROC_CID = Components.ID("{afc4a7aa-be25-4acb-bd64-999ece9b403c}");
const NS_MTMPROC_PROG_ID = "@downloadhelper.net/mp3tunes-mobile-processor;1";
const DHNS = "http://downloadhelper.net/1.0#";

var Util=null;

/**
* Object constructor
*/
function MTMProc() {
	try {
		//dump("[MTMProc] constructor\n");
		if(!Util.priorTo19()) {
			this.helper=new MTProcHelper(true);
			this.core=Components.classes["@downloadhelper.net/core;1"].
				getService(Components.interfaces.dhICore);
			this.core.registerProcessor(this);
		}
	} catch(e) {
		dump("[MTMProc] !!! constructor: "+e+"\n");
	}
}

MTMProc.prototype = {
	get name() { return "mp3tunes-mobile"; },
	get provider() { return "MP3Tunes"; },
	get title() { return Util.getText("mp3tunes.mobile-processor.title"); },
	get description() { return Util.getText("mp3tunes.mobile-processor.description"); },
	get enabled() { return this.helper.enabled; },
}

MTMProc.prototype.canHandle=function(desc) {
	//dump("[MTMProc] canHandle()\n");
	return this.helper.canHandle(desc);
}

MTMProc.prototype.requireDownload=function(desc) {
	//dump("[MTMProc] requireDownload()\n");
	return this.helper.requireDownload(desc);
}
	
MTMProc.prototype.preDownload=function(desc) {
	//dump("[MTMProc] preDownload()\n");
	return this.helper.preDownload(desc,false,false);
}

MTMProc.prototype.handle=function(desc) {
	//dump("[MTMProc] handle()\n");
	this.helper.handle(desc,false);
}

MTMProc.prototype.QueryInterface = function(iid) {
	//dump("[MTMProc] QueryInterface("+iid+")\n");
    if(
    	iid.equals(Components.interfaces.dhIProcessor) ||
    	iid.equals(Components.interfaces.nsISupports)
	) {
	    return this;
    }
	throw Components.results.NS_ERROR_NO_INTERFACE;
}

var vMTMProcModule = {
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
        compMgr.registerFactoryLocation(NS_MTMPROC_CID,
                                        "MTMProc",
                                        NS_MTMPROC_PROG_ID, 
                                        fileSpec,
                                        location,
                                        type);
    },

	unregisterSelf: function(compMgr, fileSpec, location) {
    	compMgr = compMgr.QueryInterface(Components.interfaces.nsIComponentRegistrar);
    	compMgr.unregisterFactoryLocation(NS_DH_MTMPROC_CID, fileSpec);
	},

    /*
     * The GetClassObject method is responsible for producing Factory and
     * SingletonFactory objects (the latter are specialized for services).
     */
    getClassObject: function (compMgr, cid, iid) {
        if (!cid.equals(NS_MTMPROC_CID)) {
	    	throw Components.results.NS_ERROR_NO_INTERFACE;
		}

        if (!iid.equals(Components.interfaces.nsIFactory)) {
	    	throw Components.results.NS_ERROR_NOT_IMPLEMENTED;
		}

        return this.vMTMProcFactory;
    },

    /* factory object */
    vMTMProcFactory: {
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
	
	    	if(Util==null) {
	    		Util=Components.classes["@downloadhelper.net/util-service;1"]
					.getService(Components.interfaces.dhIUtilService);
	    		try {
					var jsLoader=Components.classes["@mozilla.org/moz/jssubscript-loader;1"]
						.getService(Components.interfaces.mozIJSSubScriptLoader);
					jsLoader.loadSubScript("chrome://dwhelper/content/mp3tunes/mp3tunes-proc-helper.js");
				} catch(e) {
					dump("!!! [dhQuickDownloadProcessor] createInstance: "+e+"\n");
				}
	    	}

			return new MTMProc().QueryInterface(iid);
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
    return vMTMProcModule;
}

