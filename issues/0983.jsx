render() {
  return (
    <button
      type="button"
      className={
        this.state.reportLevel === 'producing'
          ? 'btn-default active'
          : 'btn-default'
      }
      onClick={() => { this.handleObjectTypeChange('producing'); }}
      disabled={this.state.loadingObjects}
    >
      My Button
    </button>
  )
}
