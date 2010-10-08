/******************************************************************************
 *            Copyright (c) 2006-2009 Michel Gutierrez. All Rights Reserved.
 ******************************************************************************/

/**
 * Constants.
 */

const NS_TESTPROBE_CID = Components.ID("{ae26dbda-4e3b-448a-8479-a1222dbed1a3}");
const NS_TESTPROBE_PROG_ID = "@downloadhelper.net/test-probe;1";
const DHNS = "http://downloadhelper.net/1.0#";

var Util=null;

/**
* Object constructor
*/
function TestProbe() {
	try {
		dump("[TestProbe] constructor\n");
		var core=Components.classes["@downloadhelper.net/core;1"].
        	getService(Components.interfaces.dhICore);
		core.registerProbe(this);
		this.count=0;
	} catch(e) {
		dump("[TestProbe] !!! constructor: "+e+"\n");
	}
}

TestProbe.prototype = {}

TestProbe.prototype.handleDocument=function(document,window) {
	try {
		dump("[TestProbe] handleDocument("+document.URL+")\n");
		this.count++;
		if(this.count%2)
			return;
		var desc=Components.classes["@mozilla.org/properties;1"].
			createInstance(Components.interfaces.nsIProperties);
		Util.setPropsString(desc,"label","["+this.count+"] "+document.URL);
		this.core.addEntryForDocument(desc,document,window);
	} catch(e) {
		dump("!!! [TestProbe] handleDocument: "+e+"\n");
	}
}

TestProbe.prototype.handleRequest=function(request) {
}
	
TestProbe.prototype.handleResponse=function(request) {
}

TestProbe.prototype.QueryInterface = function(iid) {
	dump("[TestProbe] QueryInterface("+iid+")\n");
    if(
    	iid.equals(Components.interfaces.dhIProbe) ||
    	iid.equals(Components.interfaces.nsISupports)
	) {
	    return this;
    }
	throw Components.results.NS_ERROR_NO_INTERFACE;
}

var vTestProbeModule = {
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
        compMgr.registerFactoryLocation(NS_TESTPROBE_CID,
                                        "TestProbe",
                                        NS_TESTPROBE_PROG_ID, 
                                        fileSpec,
                                        location,
                                        type);
    },

	unregisterSelf: function(compMgr, fileSpec, location) {
    	compMgr = compMgr.QueryInterface(Components.interfaces.nsIComponentRegistrar);
    	compMgr.unregisterFactoryLocation(NS_DH_TESTPROBE_CID, fileSpec);
	},

    /*
     * The GetClassObject method is responsible for producing Factory and
     * SingletonFactory objects (the latter are specialized for services).
     */
    getClassObject: function (compMgr, cid, iid) {
        if (!cid.equals(NS_TESTPROBE_CID)) {
	    	throw Components.results.NS_ERROR_NO_INTERFACE;
		}

        if (!iid.equals(Components.interfaces.nsIFactory)) {
	    	throw Components.results.NS_ERROR_NOT_IMPLEMENTED;
		}

        return this.vTestProbeFactory;
    },

    /* factory object */
    vTestProbeFactory: {
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

			return new TestProbe().QueryInterface(iid);
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
    return vTestProbeModule;
}

