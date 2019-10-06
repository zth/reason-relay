import { Node, Fragment } from "relay-compiler";
import { operationType } from "../generator/Types.gen";

export function makeOperationDescriptor(node: Node | Fragment): operationType {
  if (node.kind === "Root") {
    switch (node.operation) {
      case "mutation":
        return {
          tag: "Mutation",
          value: node.name
        };
      case "query":
        return {
          tag: "Query",
          value: node.name
        };
      case "subscription":
        return {
          tag: "Subscription",
          value: node.name
        };
    }
  } else if (node.kind == "Fragment") {
    return {
      tag: "Fragment",
      value: [node.name, Boolean(node.metadata && node.metadata.plural)]
    };
  }

  throw new Error("Could not map root node. This should not happen.");
}
