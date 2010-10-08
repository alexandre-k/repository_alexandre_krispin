/******************************************************************************
 *            Copyright (c) 2006-2009 Michel Gutierrez. All Rights Reserved.
 ******************************************************************************/

/**
 * Constants.
 */

const NS_FDLPROC_CID = Components.ID("{6d2d4306-a218-4be4-bdc4-61630dd7df7e}");
const NS_FDLPROC_PROG_ID = "@downloadhelper.net/flashgot-download-processor;1";
const DHNS = "http://downloadhelper.net/1.0#";

var Util=null;

/**
* Object constructor
*/
function FDLProc() {
	try {
		//dump("[FDLProc] constructor\n");
		this.flashGot=null;
		try {
			this.flashGot=Components.classes["@maone.net/flashgot-service;1"].
				getService(Components.interfaces.nsISupports).wrappedJSObject;
			this.core=Components.classes["@downloadhelper.net/core;1"].
				getService(Components.interfaces.dhICore);
			this.core.registerProcessor(this);
		} catch(e) {}

	} catch(e) {
		dump("[FDLProc] !!! constructor: "+e+"\n");
	}
}

FDLProc.prototype = {
	get name() { return "flashgot-download"; },
	get provider() { return "DownloadHelper"; },
	get title() { return Util.getText("processor.flashgot-download.title"); },
	get description() { return Util.getText("processor.flashgot-download.description"); },
	get enabled() { return true; },
}

FDLProc.prototype.canHandle=function(desc) {
	//dump("[FDLProc] canHandle()\n");
	return desc.has("media-url");
}

FDLProc.prototype.requireDownload=function(desc) {
	//dump("[FDLProc] requireDownload()\n");
	return false;
}

FDLProc.prototype.preDownload=function(desc) {
	//dump("[FDLProc] preDownload()\n");
	return false;
}

FDLProc.prototype.handle=function(desc) {
	//dump("[FDLProc] handle()\n");
	var mediaUrl=Util.getPropsString(desc,"media-url");
	var link={
			href: mediaUrl,
			description: ""
	}
    var windowMediator = Components.classes["@mozilla.org/appshell/window-mediator;1"]
    		                                .getService(Components.interfaces.nsIWindowMediator);
	var window = windowMediator.getMostRecentWindow("navigator:browser");
	window.gFlashGot.download([link],window.gFlashGotService.OP_ONE,null);
}

FDLProc.prototype.QueryInterface = function(iid) {
	//dump("[FDLProc] QueryInterface("+iid+")\n");
    if(
    	iid.equals(Components.interfaces.dhIProcessor) ||
    	iid.equals(Components.interfaces.nsISupports)
	) {
	    return this;
    }
	throw Components.results.NS_ERROR_NO_INTERFACE;
}

var vFDLProcModule = {
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
        compMgr.registerFactoryLocation(NS_FDLPROC_CID,
                                        "FDLProc",
                                        NS_FDLPROC_PROG_ID, 
                                        fileSpec,
                                        location,
                                        type);
    },

	unregisterSelf: function(compMgr, fileSpec, location) {
    	compMgr = compMgr.QueryInterface(Components.interfaces.nsIComponentRegistrar);
    	compMgr.unregisterFactoryLocation(NS_DH_FDLPROC_CID, fileSpec);
	},

    /*
     * The GetClassObject method is responsible for producing Factory and
     * SingletonFactory objects (the latter are specialized for services).
     */
    getClassObject: function (compMgr, cid, iid) {
        if (!cid.equals(NS_FDLPROC_CID)) {
	    	throw Components.results.NS_ERROR_NO_INTERFACE;
		}

        if (!iid.equals(Components.interfaces.nsIFactory)) {
	    	throw Components.results.NS_ERROR_NOT_IMPLEMENTED;
		}

        return this.vFDLProcFactory;
    },

    /* factory object */
    vFDLProcFactory: {
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
					jsLoader.loadSubScript("chrome://dwhelper/content/dlproc-helper.js");
				} catch(e) {
					dump("!!! [dhDownloadProcessor] createInstance: "+e+"\n");
				}
	    	}

			return new FDLProc().QueryInterface(iid);
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
    return vFDLProcModule;
}

