class Class {
  method() {
    // GOOD
    nodeSel
      .enter()
      .append('circle')
    const nodeSel = svg.selectAll('circle')
    // BAD
    nodeSel
                       .enter()
                       .append('circle')
  }
}
