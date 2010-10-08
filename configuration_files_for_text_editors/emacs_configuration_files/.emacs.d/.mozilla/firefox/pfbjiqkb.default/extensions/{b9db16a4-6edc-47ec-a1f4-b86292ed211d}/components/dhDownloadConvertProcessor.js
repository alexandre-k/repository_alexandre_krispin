/******************************************************************************
 *            Copyright (c) 2006-2009 Michel Gutierrez. All Rights Reserved.
 ******************************************************************************/

/**
 * Constants.
 */

const NS_DLCPROC_CID = Components.ID("{f9f662a6-77d4-437e-8f53-4fcc39fddf47}");
const NS_DLCPROC_PROG_ID = "@downloadhelper.net/download-convert-processor;1";
const DHNS = "http://downloadhelper.net/1.0#";

var Util=null;

/**
* Object constructor
*/
function DLCProc() {
	try {
		//dump("[DLCProc] constructor\n");
		this.helper=new DLProcHelper();
		this.core=Components.classes["@downloadhelper.net/core;1"].
			getService(Components.interfaces.dhICore);
		this.cvMgr=Components.classes["@downloadhelper.net/convert-manager-component"]
		              					.getService(Components.interfaces.dhIConvertMgr);
		this.core.registerProcessor(this);
	} catch(e) {
		dump("[DLCProc] !!! constructor: "+e+"\n");
	}
}

DLCProc.prototype = {
	get name() { return "convert-choice"; },
	get provider() { return "DownloadHelper"; },
	get title() { return Util.getText("processor.convert-choice.title"); },
	get description() { return Util.getText("processor.convert-choice.description"); },
	get enabled() { return true; },
}

DLCProc.prototype.canHandle=function(desc) {
	//dump("[DLCProc] canHandle()\n");
	if(!this.helper.canHandle(desc))
		return false;
	return true;
	//return this.cvMgr.checkConverter(false);
}

DLCProc.prototype.requireDownload=function(desc) {
	//dump("[DLCProc] requireDownload()\n");
	return this.helper.requireDownload(desc);
}

DLCProc.prototype.preDownload=function(desc) {
	//dump("[DLCProc] preDownload()\n");
	return this.helper.preDownload(desc,true,true);
}

DLCProc.prototype.handle=function(desc) {
	//dump("[DLCProc] handle()\n");
	this.helper.handle(desc,true);
}

DLCProc.prototype.QueryInterface = function(iid) {
	//dump("[DLCProc] QueryInterface("+iid+")\n");
    if(
    	iid.equals(Components.interfaces.dhIProcessor) ||
    	iid.equals(Components.interfaces.nsISupports)
	) {
	    return this;
    }
	throw Components.results.NS_ERROR_NO_INTERFACE;
}

var vDLCProcModule = {
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
        compMgr.registerFactoryLocation(NS_DLCPROC_CID,
                                        "DLCProc",
                                        NS_DLCPROC_PROG_ID, 
                                        fileSpec,
                                        location,
                                        type);
    },

	unregisterSelf: function(compMgr, fileSpec, location) {
    	compMgr = compMgr.QueryInterface(Components.interfaces.nsIComponentRegistrar);
    	compMgr.unregisterFactoryLocation(NS_DH_DLCPROC_CID, fileSpec);
	},

    /*
     * The GetClassObject method is responsible for producing Factory and
     * SingletonFactory objects (the latter are specialized for services).
     */
    getClassObject: function (compMgr, cid, iid) {
        if (!cid.equals(NS_DLCPROC_CID)) {
	    	throw Components.results.NS_ERROR_NO_INTERFACE;
		}

        if (!iid.equals(Components.interfaces.nsIFactory)) {
	    	throw Components.results.NS_ERROR_NOT_IMPLEMENTED;
		}

        return this.vDLCProcFactory;
    },

    /* factory object */
    vDLCProcFactory: {
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

			return new DLCProc().QueryInterface(iid);
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
    return vDLCProcModule;
}

