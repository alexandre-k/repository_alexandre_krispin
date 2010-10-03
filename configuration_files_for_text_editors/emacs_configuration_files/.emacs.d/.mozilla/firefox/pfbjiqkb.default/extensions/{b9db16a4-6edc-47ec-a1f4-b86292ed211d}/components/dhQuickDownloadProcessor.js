/******************************************************************************
 *            Copyright (c) 2006-2009 Michel Gutierrez. All Rights Reserved.
 ******************************************************************************/

/**
 * Constants.
 */

const NS_QDLPROC_CID = Components.ID("{38e2b849-ecf0-438b-b3a3-845d33f29b0c}");
const NS_QDLPROC_PROG_ID = "@downloadhelper.net/quick-download-processor;1";
const DHNS = "http://downloadhelper.net/1.0#";

var Util=null;

/**
* Object constructor
*/
function QDLProc() {
	try {
		//dump("[QDLProc] constructor\n");
		this.helper=new DLProcHelper();
		this.core=Components.classes["@downloadhelper.net/core;1"].
			getService(Components.interfaces.dhICore);
		this.core.registerProcessor(this);
	} catch(e) {
		dump("[QDLProc] !!! constructor: "+e+"\n");
	}
}

QDLProc.prototype = {
	get name() { return "quick-download"; },
	get provider() { return "DownloadHelper"; },
	get title() { return Util.getText("processor.quick-download.title"); },
	get description() { return Util.getText("processor.quick-download.description"); },
	get enabled() { return true; },
}

QDLProc.prototype.canHandle=function(desc) {
	//dump("[QDLProc] canHandle()\n");
	return this.helper.canHandle(desc);
}

QDLProc.prototype.requireDownload=function(desc) {
	//dump("[QDLProc] requireDownload()\n");
	return this.helper.requireDownload(desc);
}
	
QDLProc.prototype.preDownload=function(desc) {
	//dump("[QDLProc] preDownload()\n");
	return this.helper.preDownload(desc,false,false);
}

QDLProc.prototype.handle=function(desc) {
	//dump("[QDLProc] handle()\n");
	this.helper.handle(desc,false);
}

QDLProc.prototype.QueryInterface = function(iid) {
	//dump("[QDLProc] QueryInterface("+iid+")\n");
    if(
    	iid.equals(Components.interfaces.dhIProcessor) ||
    	iid.equals(Components.interfaces.nsISupports)
	) {
	    return this;
    }
	throw Components.results.NS_ERROR_NO_INTERFACE;
}

var vQDLProcModule = {
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
        compMgr.registerFactoryLocation(NS_QDLPROC_CID,
                                        "QDLProc",
                                        NS_QDLPROC_PROG_ID, 
                                        fileSpec,
                                        location,
                                        type);
    },

	unregisterSelf: function(compMgr, fileSpec, location) {
    	compMgr = compMgr.QueryInterface(Components.interfaces.nsIComponentRegistrar);
    	compMgr.unregisterFactoryLocation(NS_DH_QDLPROC_CID, fileSpec);
	},

    /*
     * The GetClassObject method is responsible for producing Factory and
     * SingletonFactory objects (the latter are specialized for services).
     */
    getClassObject: function (compMgr, cid, iid) {
        if (!cid.equals(NS_QDLPROC_CID)) {
	    	throw Components.results.NS_ERROR_NO_INTERFACE;
		}

        if (!iid.equals(Components.interfaces.nsIFactory)) {
	    	throw Components.results.NS_ERROR_NOT_IMPLEMENTED;
		}

        return this.vQDLProcFactory;
    },

    /* factory object */
    vQDLProcFactory: {
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
					dump("!!! [dhQuickDownloadProcessor] createInstance: "+e+"\n");
				}
	    	}

			return new QDLProc().QueryInterface(iid);
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
    return vQDLProcModule;
}

