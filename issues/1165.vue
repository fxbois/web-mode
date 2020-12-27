<template lang="pug">
  div.test
    a.link {{ a  }}
</template>

<script>
 export default {
   data() {
     return {
       a: 4
     };
   },
   methods: {
     someMethod() {
       this.logSomething();
     },
     logSomething() {
       this.asdasdasd();
       a = 4;
       return;
     }
   }
 };
</script>

<style lang="scss">
 .con {
   .link {
     color: red;
   }
 }
</style>
