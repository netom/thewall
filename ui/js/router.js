TheWall.Router.map(function() {
  this.resource('walls', { path: '/' }, function() {
    this.route('wall', { path: '/wall/:key' });
  });
});

TheWall.WallsRoute = Ember.Route.extend({
  model: function() {
    return this.store.find('wall');
  }
});

TheWall.WallsWallRoute = Ember.Route.extend({
  model: function() {
    return []; //this.store.find('wall');
  },
  renderTemplate: function(controller) {
    this.render('wall', {controller: controller});
  }
});
