macro ast_node_class_name(ast_node)
  puts {{ ast_node.class_name }}
end

ast_node_class_name(1)
ast_node_class_name("string")
ast_node_class_name(ast)
ast_node_class_name(["a", "b"])
ast_node_class_name({ key: "valu" })

macro ast_node_line_number(ast_node)
  puts {{ ast_node.line_number }}
end

ast_node_line_number 1
ast_node_line_number "string"