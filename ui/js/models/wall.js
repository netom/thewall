TheWall.Wall = DS.Model.extend({
  title: DS.attr('string'),
  key: DS.attr('string'),
  posts: DS.hasMany('post')
});

TheWall.Wall.FIXTURES = [
 {
   id: 1,
   title: 'Tibi',
   key: '298tfh29t29r929f2fr9hr3g'
 },
 {
   id: 2,
   title: 'Imre',
   key: '134tz45zhgf4tz54wgq34t4wtr'
 },
 {
   id: 3,
   title: 'Mókus',
   key: 'q35grbe6uj75uh3qhtfgq35z'
 }
];
