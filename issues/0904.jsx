render() {
  return (
    <View style={styles.composer}>
      <TextInput
        ref={textInput => {
            this.textInput = textInput;
        }}
        style={styles.textInput}
        placeholder="Enter a comment..."
        value={this.state.pendingTest}
        onChange={this.onPendingTestChanged}
        onChangeText={this.onPendingTestTextChanged}
        multiline={true}
        maxLength={1000}
      />
      <TouchableOpacity
        style={styles.buttonSend}
        onPress={this.onAddTest}
      >
        <Text style={styles.send}>Post</Text>
      </TouchableOpacity>
    </View>
  )
}
