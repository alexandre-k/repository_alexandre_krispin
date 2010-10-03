/******************************************************************************
 *            Copyright (c) 2009 Michel Gutierrez. All Rights Reserved.
 ******************************************************************************/

/**
 * Constants.
 */

const NS_DOMHOOK_CID = Components.ID("{7e757f8b-0a62-4e65-9339-4b4fd1cb9bcc}");
const NS_DOMHOOK_PROG_ID = "@downloadhelper.net/dom-hook;1";
const DHNS = "http://downloadhelper.net/1.0#";

var Util=null;

/**
* Object constructor
*/
function Hook() {
	try {
		//dump("[Hook] constructor\n");
		var prefService=Components.classes["@mozilla.org/preferences-service;1"]
		                                   .getService(Components.interfaces.nsIPrefService);
		this.pref=prefService.getBranch("dwhelper.");
		this.core=Components.classes["@downloadhelper.net/core;1"].
			getService(Components.interfaces.dhICore);
	} catch(e) {
		dump("[Hook] !!! constructor: "+e+"\n");
	}
}

Hook.prototype = {}

Hook.prototype.hook=function(document) {
	//dump("[Hook] hook("+document.URL+")\n");
	try {
		var ytInPage=this.pref.getBoolPref("yt-inpage");
		if(ytInPage) {
			this.ytHook(document);
		}
	} catch(e) {
		dump("!!! [Hook] hook("+document.URL+"): "+e+"\n");
	}
}

Hook.prototype.ytHook=function(document) {
	if(/^http:\/\/(?:[a-z]+\.)?youtube\.com\//.test(document.URL)) {
		//dump("[Hook] hook(): YouTube page\n");
		var titleH1=Util.xpGetSingleNode(document.documentElement,".//div[@id='watch-vid-title']/h1");
		if(!titleH1) {
			titleH1=Util.xpGetSingleNode(document.documentElement,".//h1[@id='watch-headline-title']");
		}
		if(!titleH1) {
			dump("!!! [Hook] hook(): title not found\n");
			return;
		}
		
		var xulNS="http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul";
				
		var img=document.createElementNS(xulNS,"xul:toolbarbutton");
		img.setAttribute("image","http://www.downloadhelper.net/favicon.ico");
		img.setAttribute("type","menu-button");
		img.style.margin="0px 12px 0px 0px";
		img.style.position="relative";
		img.style.top="-3px";
		
		var menupopup=document.createElementNS("http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul","xul:menupopup");
		menupopup.setAttribute("position","end_before");
		img.appendChild(menupopup);

		var entries=this.core.getEntriesForDocument(document);
		if(entries.length==0) {
			dump("!!! [Hook] ytHook(): no entry for: "+document.URL+"\n");
			return;
		}
		var ytEntry=this.core.cloneEntry(entries.queryElementAt(0,Components.interfaces.nsIProperties));

		function Listener(core,entry,processor) {
			this.core=core;
			this.entry=entry;
			this.processor=processor;
		}
		Listener.prototype={
			handleEvent: function(event) {
				try {
					if(this.processor.canHandle(this.entry)) {
						this.updateEntry();
						this.core.processEntry(this.processor,this.entry);
					} else {
					    var wm = Components.classes["@mozilla.org/appshell/window-mediator;1"]
					                                .getService(Components.interfaces.nsIWindowMediator);
						var window = wm.getMostRecentWindow("navigator:browser");
						window.alert(Util.getFText("yt-inpage.unavailable-processor",[this.processor.title],1));
					}
				} catch(e) {
					dump("!!! [Hook] ytHook.handleEvent(): "+e+"\n");
				}
				event.stopPropagation(); 
			},
			updateEntry: function() {
				var pageUrl=Util.getPropsString(this.entry,"page-url");
				var mediaUrl=null;
				var fileExtension=null;
				var i=this.core.getEntries().enumerate();
				while(i.hasMoreElements()) {
					var entry=i.getNext().QueryInterface(Components.interfaces.nsIProperties);
					if(entry.has("media-url") && entry.has("page-url") && entry.has("capture-method") && 
							Util.getPropsString(entry,"page-url")==pageUrl && 
							Util.getPropsString(entry,"capture-method")=="network") {
						mediaUrl=Util.getPropsString(entry,"media-url");
						fileExtension=Util.getPropsString(entry,"file-extension");
					}
				}
				if(mediaUrl) {
					this.entry=this.core.cloneEntry(this.entry);
					Util.setPropsString(this.entry,"media-url",mediaUrl);
					Util.setPropsString(this.entry,"file-name",Util.getPropsString(this.entry,"base-name")+"."+fileExtension);
				}
			}
		}

		var defProcName=this.pref.getCharPref("yt-inpage.default-processor");
		var defProcessor=null;
		
		var i=this.core.getProcessors().enumerate();
		while(i.hasMoreElements()) {
			var processor=i.getNext().QueryInterface(Components.interfaces.dhIProcessor);
			if(processor.name==defProcName)
				defProcessor=processor;
			if(processor.canHandle(ytEntry)) {
				var menuitem=document.createElementNS(xulNS,"xul:menuitem");
				menuitem.setAttribute("label",processor.title);
				menuitem.setAttribute("tooltiptext",processor.description);
				menuitem.QueryInterface(Components.interfaces.nsIDOMNSEventTarget).
					addEventListener("command",new Listener(this.core,ytEntry,processor),false,false);
				menupopup.appendChild(menuitem);
			}
		}
		if(defProcessor)
			img.QueryInterface(Components.interfaces.nsIDOMNSEventTarget).
				addEventListener("command",new Listener(this.core,ytEntry,defProcessor),false,false);
		titleH1.insertBefore(img,titleH1.firstChild);
	}
}

Hook.prototype.QueryInterface = function(iid) {
	//dump("[Hook] QueryInterface("+iid+")\n");
    if(
    	iid.equals(Components.interfaces.dhIDOMHook) ||
    	iid.equals(Components.interfaces.nsISupports)
	) {
	    return this;
    }
	throw Components.results.NS_ERROR_NO_INTERFACE;
}

var vHookModule = {
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
        compMgr.registerFactoryLocation(NS_DOMHOOK_CID,
                                        "Hook",
                                        NS_DOMHOOK_PROG_ID, 
                                        fileSpec,
                                        location,
                                        type);
    },

	unregisterSelf: function(compMgr, fileSpec, location) {
    	compMgr = compMgr.QueryInterface(Components.interfaces.nsIComponentRegistrar);
    	compMgr.unregisterFactoryLocation(NS_DH_DOMHOOK_CID, fileSpec);
	},

    /*
     * The GetClassObject method is responsible for producing Factory and
     * SingletonFactory objects (the latter are specialized for services).
     */
    getClassObject: function (compMgr, cid, iid) {
        if (!cid.equals(NS_DOMHOOK_CID)) {
	    	throw Components.results.NS_ERROR_NO_INTERFACE;
		}

        if (!iid.equals(Components.interfaces.nsIFactory)) {
	    	throw Components.results.NS_ERROR_NOT_IMPLEMENTED;
		}

        return this.vHookFactory;
    },

    /* factory object */
    vHookFactory: {
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

			return new Hook().QueryInterface(iid);
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
    return vHookModule;
}

