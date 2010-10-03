/******************************************************************************
 *            Copyright (c) 2006-2009 Michel Gutierrez. All Rights Reserved.
 ******************************************************************************/

/**
 * Constants.
 */

const NS_DLMGR_CID = Components.ID("{dc9206a8-fe97-4214-b9a7-e07e584c6710}");
const NS_DLMGR_PROG_ID = "@downloadhelper.net/download-manager;1";
const DHNS = "http://downloadhelper.net/1.0#";

var Util=null;

/**
* Object constructor
*/
function DLMgr() {
	try {
		//dump("[DLMgr] constructor\n");
		this.qDatasource=Components.classes['@mozilla.org/rdf/datasource;1?name=in-memory-datasource'].
	      	createInstance(Components.interfaces.nsIRDFDataSource);
		var prefService=Components.classes["@mozilla.org/preferences-service;1"]
			.getService(Components.interfaces.nsIPrefService);
		this.pref=prefService.getBranch("dwhelper.");
		this.counters=[];
		this.currents=[];
		this.queuedEntries={};
	} catch(e) {
		dump("[DLMgr] !!! constructor: "+e+"\n");
	}
}

DLMgr.prototype = {
	get queueDatasource() { return this.qDatasource; },
	get downloadMode() { 
		var mode="onebyone";
		try { mode=this.pref.getCharPref("download-mode"); } catch(e) {}
		return mode;
	}
}

DLMgr.prototype.download=function(listener,entry,ctx) {
	//dump("[DLMgr] download()\n");
	switch(this.downloadMode) {
		case "normal":
			this.doDownload(listener,entry,ctx);
			break;
		case "onebyone":
		case "controlled":
			this.queueDownload(listener,entry,ctx);
			break;
	}
}

DLMgr.prototype.doDownload=function(listener,entry,ctx) {
	
	try {
	
	//dump("[DLMgr] doDownload()\n");
	var file=null;
	if(entry.has("dl-file")) {
		file=entry.get("dl-file",Components.interfaces.nsIFile);
	} else {
	 	file=Components.classes["@mozilla.org/file/directory_service;1"]
	 	                        .getService(Components.interfaces.nsIProperties)
	 	                        .get("TmpD", Components.interfaces.nsIFile);
	 	if(entry.has("file-name"))
	 		file.append(Util.getPropsString(entry,"file-name"));
	 	else
	 		file.append("dwhelper-dl");
	 	file.createUnique(Components.interfaces.nsIFile.NORMAL_FILE_TYPE, 0644);
	 	entry.set("dl-file",file);
	}
	
	var url = Components.classes["@mozilla.org/network/standard-url;1"].createInstance(Components.interfaces.nsIURI);
	url.spec = Util.getPropsString(entry,"media-url");
	var fileURL = makeFileURI(file);
	var persist = makeWebBrowserPersist();
	
	const nsIWBP = Components.interfaces.nsIWebBrowserPersist;	
	persist.persistFlags = nsIWBP.PERSIST_FLAGS_REPLACE_EXISTING_FILES | nsIWBP.PERSIST_FLAGS_FROM_CACHE;
	persist.persistFlags |= nsIWBP.PERSIST_FLAGS_AUTODETECT_APPLY_CONVERSION;
	
	var tr = Components.classes["@mozilla.org/transfer;1"].createInstance(Components.interfaces.nsITransfer);

	var progress=new Progress(tr,this,listener,entry,ctx);

	persist.progressListener = progress;
	
	var referrer=Util.getPropsString(entry,"referrer");
	if(referrer!=null) {
		var refStr=referrer;	
    	referrer = Components.classes["@mozilla.org/network/standard-url;1"].createInstance(Components.interfaces.nsIURI);
    	referrer.spec = refStr;
    }

	persist.saveURI(url,null, referrer, null, null,fileURL);
	tr.init(url,fileURL, "", null, null, null, persist);

	} catch(e) {
		dump("!!! [DLMgr] doDownload(): "+e+"\n");
	}
}

DLMgr.prototype.queueDownload=function(listener,entry,ctx) {
	//dump("[DLMgr] queueDownload()\n");
	try {
		var dEntry=Util.createAnonymousNodeS(this.qDatasource,"urn:root");
		Util.setPropsString(entry,"download-node-value",dEntry.Value);
		//dump("[DLMgr] queued "+dEntry.Value+"\n");
		var label;
		if(entry.has("cv-file")) {
			label=entry.get("cv-file",Components.interfaces.nsILocalFile).leafName;
		} else if(entry.has("dl-file")) {
			label=entry.get("dl-file",Components.interfaces.nsILocalFile).leafName;
		} else {
			label=Util.getPropsString(entry,"label")
		}
		Util.setPropertyValueRS(this.qDatasource,dEntry,DHNS+"label",label);
		Util.setPropertyValueRS(this.qDatasource,dEntry,DHNS+"status","queued");
		this.queuedEntries[dEntry.Value]={
				listener: listener,
				entry: entry,
				ctx: ctx
		}
		this.checkTransfer();	
	} catch(e) {
		dump("!!! [DLMgr] queueDownload(): "+e+"\n");
	}
}

DLMgr.prototype.checkTransfer=function() {
	//dump("[DLMgr] checkTransfer()\n");
	var maxDL=1;
	if(this.downloadMode=="controlled") {
		maxDL=this.pref.getIntPref("download.controlled.max");
		if(maxDL<1) {
			maxDL=1;
			this.pref.setIntPref("download.controlled.max",maxDL);
		}
	}
	var entries=Util.getChildResourcesS(this.qDatasource,"urn:root",{});
	while(this.currents.length<maxDL && entries.length>0) {
		var dEntry=null;
		for(var i in entries) {
			var status=Util.getPropertyValueRS(this.qDatasource,entries[i],DHNS+"status");
			if(status=="queued") {
				dEntry=entries[i];
				break;
			}
		}
		if(dEntry==null)
			break;
		var data=this.queuedEntries[dEntry.Value];
		Util.setPropertyValueRS(this.qDatasource,dEntry,DHNS+"status","downloading");
		//dump("[DLMgr] starting "+dEntry.Value+"\n");
		this.doDownload(data.listener,data.entry,data.ctx);
		this.currents.push(dEntry.Value);
	}
	var count=entries.length;
	for(var i in this.counters) {
		if(count==0)
			this.counters[i].setAttribute("value","");
		else
			this.counters[i].setAttribute("value","("+count+")");
	}
}

DLMgr.prototype.transferDone = function(status,request,listener,entry,ctx) {
	//dump("[DLMgr] transferDone()\n");

	var code=0;
	try {
		var hc=request.QueryInterface(Components.interfaces.nsIHttpChannel);
		code=hc.responseStatus;
	} catch(e) {}

	try {

		if(status==0 && code==200) {
			this.incrementDownloadCount();
		}
		
		var nodeValue=Util.getPropsString(entry,"download-node-value");
		
		//dump("[DLMgr] done "+nodeValue+"\n");
		delete this.queuedEntries[nodeValue];
	
		Util.removeChildSS(this.qDatasource,"urn:root",nodeValue);
		Util.removeReferenceS(this.qDatasource,nodeValue);			

		for(var i in this.currents) {
			if(this.currents[i]==nodeValue) {
				//dump("[DLMgr] purged "+nodeValue+"\n");
				this.currents.splice(i,1);
				break;
			}
		}

		this.checkTransfer();
	
	} catch(e) {
		dump("!!! [DLMgr] transferDone(): "+e+"\n");
	}

	try {
		if(listener)
			listener.downloadFinished(status,request,entry,ctx);
	} catch(e) {
		dump("!!! [DLMgr] transferDone()/downloadFinished(): "+e+"\n");
	}
}

DLMgr.prototype.incrementDownloadCount = function() {
	var dwcount=0;
	try {
		dwcount=this.pref.getIntPref("download-count");
	} catch(e) {
	}
	dwcount++;
	this.pref.setIntPref("download-count",dwcount);
	if(dwcount%100==0) {
		this.donate(dwcount);
	}
	if(this.pref.getBoolPref("disable-dwcount-cookie")==false) {
		try {
			var cMgr = Components.classes["@mozilla.org/cookiemanager;1"].
	           getService(Components.interfaces.nsICookieManager2);
	        try {
				cMgr.add(".downloadhelper.net","/","dwcount",""+dwcount,false,true,new Date().getTime()/1000+10000000);
				cMgr.add(".vidohe.com","/","dwcount",""+dwcount,false,true,new Date().getTime()/1000+10000000);
			} catch(e) {
				cMgr.add(".downloadhelper.net","/","dwcount",""+dwcount,false,true,false,new Date().getTime()/1000+10000000);
				cMgr.add(".vidohe.com","/","dwcount",""+dwcount,false,true,false,new Date().getTime()/1000+10000000);
			}
		} catch(e) {
			dump("!!! [DhDownloadMgr] incrementDownloadCount() "+e+"\n");
		}
	}
}

DLMgr.prototype.donate=function(count) {
	if(this.pref.getBoolPref("donate-not-again"))
		return;
	try {
		var cvMgr=Components.classes["@downloadhelper.net/convert-manager-component"]
		              					.getService(Components.interfaces.dhIConvertMgr);
		var cvInfo=cvMgr.getInfo();
		if(cvInfo.get("windows",Components.interfaces.nsISupportsPRBool).data==true &&
				cvInfo.get("unregistered",Components.interfaces.nsISupportsPRBool).data==false)
			return; // don't request donation to those who have a license
	} catch(e) {}
    var options="chrome,centerscreen";
    try {
        var wm = Components.classes["@mozilla.org/appshell/window-mediator;1"]
                  .getService(Components.interfaces.nsIWindowMediator);
		var w = wm.getMostRecentWindow("navigator:browser");
	    w.open('chrome://dwhelper/content/donate.xul','dwhelper-dialog',options);
	} catch(e) {
		dump("!!! [DhDownloadMgr] donate() "+e+"\n");
	}
}

DLMgr.prototype.getDefaultDir=function() {
	var file=null;
	try {
		file = Components.classes["@mozilla.org/file/directory_service;1"]
	                     .getService(Components.interfaces.nsIProperties)
	                     .get("Home", Components.interfaces.nsIFile);
	} catch(e) {
    	try {
			file=Components.classes["@mozilla.org/file/directory_service;1"]
		    	.getService(Components.interfaces.nsIProperties)
		        .get("TmpD", Components.interfaces.nsIFile);
		} catch(e) {
		}
	}
	if(!file.exists()) {
		throw(DWHUtil.getText("error.nohome"));
	}
	file.append("dwhelper");
	return file;
}

DLMgr.prototype.getDownloadDirectory=function() {

	var fileName=Util.getUnicharPref(this.pref,"storagedirectory",null);
	
	var file;
	if(fileName==null || fileName.length==0) {
		file=this.getDefaultDir();
	} else {
		try {
		    file=Components.classes["@mozilla.org/file/local;1"].
		        createInstance(Components.interfaces.nsILocalFile);
		    file.initWithPath(fileName);
		    if(file.exists()==false || file.isWritable()==false || file.isDirectory()==false)
		    	file=this.getDefaultDir();
		} catch(e) {
	    	file=this.getDefaultDir();
		}
	}
	if(!file.exists()) {
		file.create(Components.interfaces.nsIFile.DIRECTORY_TYPE, 0755);
	}
	Util.setUnicharPref(this.pref,"storagedirectory",file.path);
	return file;
}

DLMgr.prototype.setDownloadDirectory=function(fileDir) {
	if(!fileDir.isDirectory())
		fileDir=fileDir.parent;
	Util.setUnicharPref(this.pref,"storagedirectory",fileDir.path);
}

DLMgr.prototype.removeFromQueue=function(entries,length) {
	for(var i in entries) {
		var entry=entries[i];
		var status=Util.getPropertyValueRS(this.qDatasource,entry,DHNS+"status");
		if(status=="queued") {
			delete this.queuedEntries[entry.Value]
			Util.removeChildSR(this.qDatasource,"urn:root",entry);
			Util.removeReference(this.qDatasource,entry);			
		}
	}
}

DLMgr.prototype.registerCounter = function(counter) {
	//dump("[DLMgr] registerCounter()\n");
	this.counters.push(counter);
}

DLMgr.prototype.unregisterCounter = function(counter) {
	//dump("[DLMgr] unregisterCounter()\n");
	this.counters.splice(this.counters.indexOf(counter),1);
}

DLMgr.prototype.QueryInterface = function(iid) {
	//dump("[DLMgr] QueryInterface("+iid+")\n");
    if(
    	iid.equals(Components.interfaces.dhIDownloadMgr) ||
    	iid.equals(Components.interfaces.nsISupports)
	) {
	    return this;
    }
	throw Components.results.NS_ERROR_NO_INTERFACE;
}

var vDLMgrModule = {
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
        compMgr.registerFactoryLocation(NS_DLMGR_CID,
                                        "DLMgr",
                                        NS_DLMGR_PROG_ID, 
                                        fileSpec,
                                        location,
                                        type);
    },

	unregisterSelf: function(compMgr, fileSpec, location) {
    	compMgr = compMgr.QueryInterface(Components.interfaces.nsIComponentRegistrar);
    	compMgr.unregisterFactoryLocation(NS_DH_DLMGR_CID, fileSpec);
	},

    /*
     * The GetClassObject method is responsible for producing Factory and
     * SingletonFactory objects (the latter are specialized for services).
     */
    getClassObject: function (compMgr, cid, iid) {
        if (!cid.equals(NS_DLMGR_CID)) {
	    	throw Components.results.NS_ERROR_NO_INTERFACE;
		}

        if (!iid.equals(Components.interfaces.nsIFactory)) {
	    	throw Components.results.NS_ERROR_NOT_IMPLEMENTED;
		}

        return this.vDLMgrFactory;
    },

    /* factory object */
    vDLMgrFactory: {
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
					jsLoader.loadSubScript("chrome://global/content/contentAreaUtils.js");
				} catch(e) {
					dump("!!! [dhDownloadMgr] createInstance: "+e+"\n");
				}
	    	}

			return new DLMgr().QueryInterface(iid);
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
    return vDLMgrModule;
}

/*---------------------------------------------------------------------------------------*/

function Progress(tr,observer,listener,entry,ctx) {
	this.tr=tr;
	this.observer=observer;
	this.listener=listener;
	this.entry=entry;
	this.ctx=ctx;
}

Progress.prototype.onLocationChange=function(webProgress, request, location ) {
	this.tr.onLocationChange(webProgress, request, location);
}

Progress.prototype.onProgressChange=function(webProgress, request, curSelfProgress, maxSelfProgress, curTotalProgress, maxTotalProgress ) {
	try {
		this.tr.onProgressChange(webProgress, request, curSelfProgress, maxSelfProgress, curTotalProgress, maxTotalProgress );
	} catch(e) {}
}

Progress.prototype.onSecurityChange=function(webProgress, request, state ) {
	this.tr.onSecurityChange(webProgress, request, state );
}

Progress.prototype.onStateChange=function(webProgress, request, stateFlags, status ) {
	this.tr.onStateChange(webProgress, request, stateFlags, status );
	if(stateFlags & Components.interfaces.nsIWebProgressListener.STATE_STOP) {
		this.observer.transferDone(status,request,this.listener,this.entry,this.ctx);
	}
}

Progress.prototype.onStatusChange=function(webProgress, request, status, message ) {
	this.tr.onStatusChange(webProgress, request, status, message );
}

