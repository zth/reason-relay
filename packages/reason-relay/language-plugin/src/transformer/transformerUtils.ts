import {
  Node,
  Fragment,
  Selection,
  LocalArgumentDefinition
} from "relay-compiler";
import { GraphQLCompositeType, GraphQLObjectType } from "graphql";
import { operationType } from "./TypesTransformer.gen";

// @ts-ignore
export { lookupPropAtPath } from "./lookupPropAtPath";

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

export declare function lookupPropAtPath(
  root: Node | Fragment,
  path: string[]
): Selection | LocalArgumentDefinition | null;
