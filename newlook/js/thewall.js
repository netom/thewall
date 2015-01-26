var Builder = function(body) {
  var constructor = function() {    
      this.init.apply(this, arguments);          
  };  

  for (var property in body) { 
     constructor.prototype[property] = body[property];
  }
        
  if (!constructor.prototype.init) constructor.prototype.init = function(){};      
  
  return constructor;    
}

var WallStorage = Builder({
  init: function() {
    this.storage = $.initNamespaceStorage('thewall').localStorage;
    if (!this.storage) {
      this.storage = $.initNamespaceStorage('thewall').sessionStorage;
    }
    if (!this.storage) {
      this.storage = $.initNamespaceStorage('thewall').cookieStorage;
    }
  },

  getWalls: function() {
    var walls = this.storage.get('walls');

    if (!walls) {
      walls = [];
      this.setWalls(walls);
    }

    return walls;
  },

  setWalls: function(walls) {
    this.storage.set('walls', walls);
  },

  addWall: function(wall) {
    this.storage.set('walls', this.storage.get('walls').concat([wall]));
  },

  removeWall: function(wall) {
    var newWalls = [];

    for (var w in this.getWalls()) {
      if (w != wall) {
        newWalls.push(w);
      }
    }

    this.setWalls(newWalls);
  }

});

var WallMenu = Builder({
  init: function(self) {
    $(function() {
      var navmenu = $('nav#menu');

      navmenu.mmenu({
        classes: "mm-light"
      });

      $("li#addnew").on( "click", function(e) {
        prompt( "The menu has just been selected.");
        e.stopPropagation();
        e.preventDefault();
      });
    });
  },

  onWallLink: function (listener) {
  },

  onAddNew: function (listener) {
  },

  onAddKey: function (listener) {
  },

  onRemove: function (listener) {
  }

});

var WallImport = Builder({
  init: function(self) {
    if (FileReader)
    {
      var self = this;
      $(function() {
        document.getElementById('fileinput').addEventListener('change', self.readImportFile, false);
      });
    }
  },

  /* 
   * Reads a file with data to import
   * */
  readImportFile: function(evt) {
    //Retrieve the first (and only!) File from the FileList object
    var f = evt.target.files[0]; 

    if (f) {
      var r = new FileReader();
      r.onload = function(e) { 
          var contents = e.target.result;
        alert( "Got the file.n" 
              +"name: " + f.name + "\n"
              +"type: " + f.type + "\n"
              +"size: " + f.size + " bytes\n"
              + "starts with: " + contents.substr(0, contents.indexOf("\n"))
        );  
      }
      r.readAsText(f);
    } else { 
      alert("Failed to load file");
    }
  },
});

var WallExport = Builder({
  init: function(self) {
    var blob = new Blob(["Hello, world!"], {type: "text/plain;charset=utf-8"});
    saveAs(blob, "hello world.txt");
  }
});

var WallBackend = Builder({
  init: function(self) {
  }
});

var WallApp = Builder({
  init: function(self) {
    this.storage = new WallStorage();
    this.import = new WallImport();
    this.export = new WallExport();

    this.menu = new WallMenu(this.storage, this.import, this.export);

    this.backend = new WallBackend();

    this.menu.onWallLink(this.wallLinkClicked);
    this.menu.onAddNew(this.addNew);
    this.menu.onAddKey(this.addKey);
    this.menu.onRemove(this.remove);
  },

  wallLinkClicked: function() {
    // Swap in div
  },

  addNew: function() {
  },

  addKey: function() {
  },

  remove: function() {
  }
});

var wallApp = new WallApp();

/* vim: set tabstop=2:softtabstop=2:shiftwidth=2:expandtab */
