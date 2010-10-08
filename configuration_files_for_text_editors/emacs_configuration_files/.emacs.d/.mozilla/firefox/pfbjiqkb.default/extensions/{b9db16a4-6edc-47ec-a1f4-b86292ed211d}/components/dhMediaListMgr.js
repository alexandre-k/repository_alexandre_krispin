/******************************************************************************
 *            Copyright (c) 2006-2009 Michel Gutierrez. All Rights Reserved.
 ******************************************************************************/

/**
 * Constants.
 */

const NS_DH_MEDIA_LIST_MGR_CID = Components.ID("{55f8f6d3-9ac7-4046-b1bb-7c732e27d2d6}");
const NS_DH_MEDIA_LIST_MGR_PROG_ID = "@downloadhelper.net/media-list-manager";
const DHNS = "http://downloadhelper.net/1.0#";

var Util=null;

/**
* Object constructor
*/
function DhMediaListMgr() {
	try {
		//dump("[DhMediaListMgr] constructor\n");
		
		this.timer=null;
		this.needSaving=false;
		this.currentURLs=[];
		var prefService=Components.classes["@mozilla.org/preferences-service;1"]
			.getService(Components.interfaces.nsIPrefService);
		this.pref=prefService.getBranch("dwhelper.");

		this.storageFile = Components.classes["@mozilla.org/file/directory_service;1"]
    		.getService(Components.interfaces.nsIProperties)
        	.get("ProfD", Components.interfaces.nsIFile);
        this.storageFile.append("dh-media-lists.rdf");
		if(this.storageFile.exists() && this.pref.getBoolPref("history-enabled")) {
			this.dataSource=Util.getDatasourceFromRDFFile(this.storageFile);
		} else {
			this.dataSource=Components.classes
	      		['@mozilla.org/rdf/datasource;1?name=in-memory-datasource'].
	          		createInstance(Components.interfaces.nsIRDFDataSource);
			var hist=Util.createNodeSS(this.dataSource,"urn:root",DHNS+"history-list");
			Util.setPropertyValueRS(this.dataSource,hist,DHNS+"name",Util.getText("medialist.history"));
			this.needSaving=true;
		}

		this.observerService =
			Components.classes["@mozilla.org/observer-service;1"]
				.getService(Components.interfaces.nsIObserverService);
		this.observerService.addObserver(this,"quit-application",false);
		
	} catch(e) {
		dump("!!! [DhMediaListMgr] constructor: "+e+"\n");
	}
}

DhMediaListMgr.prototype = {}

DhMediaListMgr.prototype.initTimer=function() {
	//dump("[DhMediaListMgr] initTimer()\n");
	if(this.timer==null) {
		this.timer=Components.classes['@mozilla.org/timer;1'].
         		createInstance(Components.interfaces.nsITimer);
		this.timer.init(this,60*1000,Components.interfaces.nsITimer.TYPE_REPEATING_SLACK);
	}
}

DhMediaListMgr.prototype.getDataSource=function() {
	return this.dataSource;
}

DhMediaListMgr.prototype.addToList=function(list,url,type,pageurl,filename,referer) {
	//dump("[DhMediaListMgr] addToList("+list+","+url+","+type+","+pageurl+","+filename+","+referer+")\n");
	if(!this.pref.getBoolPref("history-enabled"))
		return null;
	try {
	if(Util.getPropertyValueSS(this.dataSource,list,DHNS+"name")==null) {
		dump("!!! [DhMediaListMgr] addToList(): list "+list+" does not exist\n");
		return null;
	}
	var entries=Util.getChildResourcesS(this.dataSource,list,{});
	for(var i=0;i<entries.length;i++) {
		if(Util.getPropertyValueRS(this.dataSource,entries[i],DHNS+"url")==url) {
			//dump("[DhMediaListMgr] !!! addToList(): removing previous entry\n");
			Util.removeReference(this.dataSource,entries[i]);
		}
	}
	this.needSaving=true;
	var entry=Util.createAnonymousNodeS(this.dataSource,list);
	Util.setPropertyValueRS(this.dataSource,entry,DHNS+"url",url);
	if(type!=null)
		Util.setPropertyValueRS(this.dataSource,entry,DHNS+"type",type);
	if(pageurl!=null)
		Util.setPropertyValueRS(this.dataSource,entry,DHNS+"pageurl",pageurl);
	var date=new Date();
	Util.setPropertyValueRS(this.dataSource,entry,DHNS+"date",""+date.getTime());
	Util.setPropertyValueRS(this.dataSource,entry,DHNS+"datestr",""+date.toLocaleString());
	Util.setPropertyValueRS(this.dataSource,entry,DHNS+"filename",""+filename);
	if(referer!=null) {
		Util.setPropertyValueRS(this.dataSource,entry,DHNS+"referer",""+referer);
	}
	
	return entry.Value;
	} catch(e) {
		dump("!!! [DhMediaListMgr] addToList(): "+e+"\n");
		return null;
	}
}

DhMediaListMgr.prototype.removeFromList=function(list,media) {
	try {
	if(Util.getPropertyValueSS(this.dataSource,list,DHNS+"name")==null) {
		dump("!!! [DhMediaListMgr] removeFromList(): list "+list+" does not exist\n");
		return null;
	}
	Util.removeReferenceS(this.dataSource,media);
	this.needSaving=true;
	} catch(e) {
		dump("!!! [DhMediaListMgr] removeFromList(): "+e+"\n");
	}
}

DhMediaListMgr.prototype.saveToFile=function() {
	//dump("[DhMediaListMgr] saveToFile\n");
	try {
		var serializer="@mozilla.org/rdf/xml-serializer;1";
		var s=Components.classes[serializer].createInstance(Components.interfaces.nsIRDFXMLSerializer);
		s.init(this.dataSource);
		var stream = Components.classes['@mozilla.org/network/file-output-stream;1']
		    .createInstance(Components.interfaces.nsIFileOutputStream);
		stream.init(this.storageFile, 42, 0644, 0); 
	
		s.QueryInterface(Components.interfaces.nsIRDFXMLSource).Serialize(stream);
		stream.close();
	} catch(e) {
		dump("!!! [DhMediaListMgr] saveToFile: "+e+"\n");
	}
}

DhMediaListMgr.prototype.checkSave=function() {
	//dump("[DhMediaListMgr] checkSave\n");
	if(this.needSaving) {
		this.needSaving=false;
		this.saveToFile();
	}
}

DhMediaListMgr.prototype.cleanupCurrentURL = function() {
	var d=new Date().getTime()/1000;
	for(var i=this.currentURLs.length-1;i>=0;i--) {
		var entry=this.currentURLs[i];
		if(d>entry.expire) {
			//dump("[DhMediaListMgr] cleanupCurrentURL("+entry.url+")\n");
			this.currentURLs.splice(i,1);
		}
	}
}

DhMediaListMgr.prototype.addCurrentURL = function(item) {
	var expire=new Date().getTime/1000+60;
	for(var i=0;i<this.currentURLs.length;i++) {
		var entry=this.currentURLs[i];
		if(entry.url==item) {
			//dump("[DhMediaListMgr] addCurrentURL("+entry.url+") updated\n");
			entry.expire=expire;
			return;
		}
	}
	//dump("[DhMediaListMgr] addCurrentURL("+item+") added\n");
	this.currentURLs.push({
		url: item,
		expire: expire
	});
}

DhMediaListMgr.prototype.removeCurrentURL = function(item) {
	for(var i=0;i<this.currentURLs.length;i++) {
		var entry=this.currentURLs[i];
		if(entry.url==item) {
			this.currentURLs.splice(i,1);
			//dump("[DhMediaListMgr] removeCurrentURL("+entry.url+") removed\n");
			return;
		}
	}
}

DhMediaListMgr.prototype.checkCurrentURL = function(item) {
	this.cleanupCurrentURL();
	for(var i=0;i<this.currentURLs.length;i++) {
		var entry=this.currentURLs[i];
		if(entry.url==item) {
			//dump("[DhMediaListMgr] checkCurrentURL("+entry.url+") found\n");
			return true;
		}
	}	
	//dump("[DhMediaListMgr] checkCurrentURL("+item+") (out of "+this.currentURLs.length+") not found\n");
	return false;
}

DhMediaListMgr.prototype.observe=function(subject,topic,data) {
	//dump("[DhMediaListMgr] observe("+subject+","+topic+","+data+")\n");
	if(topic=="timer-callback") {
		this.checkSave();
	} else if(topic=="quit-application") {
		this.checkSave();
		var coe=true;
		try {
			coe=this.pref.getBoolPref("history-clearonexit");
		} catch(e) {}
		if(coe)
			this.clearHistory();
		this.observerService.removeObserver(this,"quit-application");
	}
}

DhMediaListMgr.prototype.clearHistory = function() {
	try {
		var hEntries=Util.getChildResourcesS(this.dataSource,DHNS+"history-list",{});
		for(var i in hEntries) {
			var entry=hEntries[i];
			Util.removeChildSR(this.dataSource,DHNS+"history-list",entry);
			Util.removeReference(this.dataSource,entry);
		}
		this.saveToFile();
	} catch(e) {
		dump("!!! [DhMediaListMgr] clearHistory: "+e+"\n");
	}
}


DhMediaListMgr.prototype.QueryInterface = function(iid) {
	//dump("[DhMediaListMgr] QueryInterface("+iid+")\n");
    if(
    	iid.equals(Components.interfaces.nsITimerCallback)==false &&
    	iid.equals(Components.interfaces.nsIObserver)==false &&
    	iid.equals(Components.interfaces.dhIMediaListMgr)==false &&
    	iid.equals(Components.interfaces.nsISupports)==false
	) {
            throw Components.results.NS_ERROR_NO_INTERFACE;
        }
    return this;
}


var vDhMediaListMgrModule = {
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
        //dump("*** Registering DhMediaListMgr\n");
        compMgr = compMgr.QueryInterface(Components.interfaces.nsIComponentRegistrar);
        compMgr.registerFactoryLocation(NS_DH_MEDIA_LIST_MGR_CID,
                                        "DhMediaListMgr",
                                        NS_DH_MEDIA_LIST_MGR_PROG_ID, 
                                        fileSpec,
                                        location,
                                        type);
        //dump("*** Registered DhMediaListMgr\n");
    },

	unregisterSelf: function(compMgr, fileSpec, location) {
    	compMgr = compMgr.QueryInterface(Components.interfaces.nsIComponentRegistrar);
    	compMgr.unregisterFactoryLocation(NS_DH_MEDIA_LIST_MGR_CID, fileSpec);
	},

    /*
     * The GetClassObject method is responsible for producing Factory and
     * SingletonFactory objects (the latter are specialized for services).
     */
    getClassObject: function (compMgr, cid, iid) {
        if (!cid.equals(NS_DH_MEDIA_LIST_MGR_CID)) {
	    	throw Components.results.NS_ERROR_NO_INTERFACE;
		}

        if (!iid.equals(Components.interfaces.nsIFactory)) {
	    	throw Components.results.NS_ERROR_NOT_IMPLEMENTED;
		}

        return this.vDhMediaListMgrFactory;
    },

    /* factory object */
    vDhMediaListMgrFactory: {
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
	
			//dump("DhMediaListMgr: create instance\n");

	    	if(Util==null) 
	    		Util=Components.classes["@downloadhelper.net/util-service;1"]
					.getService(Components.interfaces.dhIUtilService);

			return new DhMediaListMgr().QueryInterface(iid);
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
    return vDhMediaListMgrModule;
}

