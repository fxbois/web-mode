<my-tag>
  <h1>{ opts.title }</h1>
  <script>
   this.items = [
     { title: 'First' }, { title: 'Second' }
   ];
   remove(event) {

   }
  </script>
</my-tag>

<todo>
  <div each={ items }>
    <h3>{ title }</h3>
    <a onclick={ parent.remove }>Remove</a>
  </div>
  <script>
   this.items = [
     { title: 'First' }, { title: 'Second' }
   ];
   remove(event) {

   }
  </script>
</todo>
