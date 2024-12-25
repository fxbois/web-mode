export default function TreeView({ data }: TreeProps) {
  return (
    <Tree
      data={data}
      levelOffset={23}
      renderNode={({ node, expanded, hasChildren, elementProps }: RenderTreeNodePayload) => (
        <Group gap={5} {...elementProps}>
          {hasChildren && (
            <>
              <IconChevronDown
                size={18}
                style={{ transform: expanded ? 'rotate(180deg)' : 'rotate(0deg)' }}
              />
              <span>{node.label}</span>
            </>
          )}
          {!hasChildren && <NoteButton>{node.label}</NoteButton>}
        </Group>
      )}
    />
  );
}
