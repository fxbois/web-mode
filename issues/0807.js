Relay.createContainer(Story, {
  initialVariables: {
    numCommentsToShow: 10,
    showComments: false,
  },
  fragments: {
    story: (variables) => Relay.QL`
      fragment on Story {
        comments(first: $numCommentsToShow) @include(if: $showComments) {
          edges {
            node {
              author { name },
              id,
              text,
            },
          },
        },
      }
    `,
  }
});

// An inline fragment - useful in small quantities, but best not to share
// between modules.
var userFragment = Relay.QL`
  fragment on User {
    name,
  }
`;
Relay.createContainer(Story, {
  fragments: {
    bar: () => Relay.QL`
      fragment on Story {
        author {
          # Fetch the same information about the story's author ...
          ${userFragment},
        },
        comments {
          edges {
            node {
              author {
                # ... and the authors of the comments.
                ${userFragment},
              },
            },
          },
        },
      }
    `,
  }
});
