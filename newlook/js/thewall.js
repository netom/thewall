/*
 * Returns a constructor.
 * 
 * The parameter is an object whose properties are
 * copied into the constructor's prototype.
 * */
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

var Wall = Builder({
  init: function(key, title, count) {
    this.key = key;
    this.title = title;
    this.count = count;
  },

  getKey: function() {
    return this.key;
  },

  getTitle: function() {
    return this.title;
  },

  setTitle: function(title) {
    this.title = title;
  },

  getCount: function() {
    return this.count;
  },

  setCount: function(count) {
    this.count = count;
  }
});

/*
 * Local storage
 * 
 * Either by "true" local storage, session storage, or cookies.
 * */
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

  getAll: function() {
    var walls = this.storage.get('walls');

    if (!walls) {
      this.setWalls([]);
    }

    var ret = [];

    for (var i in walls) {
      // Turn plain JSON objects into Wall objects
      ret.push(new Wall(walls[i].key, walls[i].title, walls[i].count));
    }

    return ret;
  },

  setAll: function(walls) {
    this.storage.set('walls', walls);
  },

  add: function(wall) {
    var walls = this.getAll();

    for (i in walls) {
      if (walls[i].getKey() == wall.getKey()) {
        return;
      }
    }

    walls.push(wall);

    this.storage.set('walls', walls); // TODO: do we need this?
  },

  remove: function(wall) {
    var walls = this.getAll();
    var newWalls = [];

    for (var i in walls) {
      if (walls[i].getKey() != wall.getKey()) {
        newWalls.push(walls[i]);
      }
    }

    this.setAll(newWalls);
  }

});

/*
 * Menu handling
 * */
var WallMenu = Builder({
  init: function(storage) {
    this.storage = storage;

    var self = this;

    $(function() {
      var navmenu = $('nav#menu');

      navmenu.mmenu({
        classes: "mm-light"
      });

      $("#addnew").on( "click", function(e) {
        prompt( "The menu has just been selected.");
        e.stopPropagation();
        e.preventDefault();
      });

      self.loadMenu();

    });

  },

  loadMenu: function() {
    var walls = this.storage.getAll();
    for (var i in walls) {
      this.add(walls[i]);
    }
  },

  /*
   * Add a new wall link to the menu
   * */
  add: function(wall) {
    this.storage.add(wall);

    var html = '<li class="wall"><a href="#' + wall.getKey() + '"><img src="img/wireframe_mono/blacks/16x16/spechbubble_2.png"/> ';
    if (wall.getCount() > 0) {
      html += '<strong>' + wall.getTitle() + ' (Zg9mDA1IXmPejy3KmANf7oq3Cj0ouqEa)</strong>' + wall.getCount() + '</a></li>';
    } else {
      html += wall.getTitle() + ' (' + wall.getKey() + ') ' + wall.getCount() + '</a></li>';
    }

    var nowalls = $('#nowalls');

    nowalls.hide();
    nowalls.after(html);
  },

  remove: function(key) {
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
    //var blob = new Blob(["Hello, world!"], {type: "text/plain;charset=utf-8"});
    //saveAs(blob, "hello world.txt");
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
