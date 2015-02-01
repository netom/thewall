TheWall.Wall = DS.Model.extend({
  title: DS.attr('string'),
  key: DS.attr('string'),
  posts: DS.hasMany('post')
});

TheWall.Wall.FIXTURES = [
 {
   id: 1,
   title: 'Tibi',
   key: ''
 },
 {
   id: 2,
   title: 'Imre',
   key: ''
 },
 {
   id: 3,
   title: 'Mókus',
   key: ''
 }
];
