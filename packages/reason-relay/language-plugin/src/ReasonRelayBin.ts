import { spawnSync } from "child_process";
import * as path from "path";

interface GenerateFromFlowConfig {
  content: string;
  operation_type:
    | {
        operation: "Query";
        operation_value: string;
      }
    | {
        operation: "Mutation";
        operation_value: string;
      }
    | {
        operation: "Subscription";
        operation_value: string;
      }
    | {
        operation: "Fragment";
        fragment_value: [string, boolean];
      };
  print_config: {
    connection: null | {
      key: string;
      at_object_path: string[];
      field_name: string;
    };
  };
}

export const generateFromFlowTypes = (
  config: GenerateFromFlowConfig
): string => {
  return spawnSync(
    path.resolve(path.join(__dirname, "./ReasonRelayBin.exe")),
    ["generate-from-flow"],
    {
      cwd: __dirname,
      stdio: "pipe",
      encoding: "utf-8",
      input: JSON.stringify(config),
    }
  )
    .output.filter(Boolean)
    .join("");
};
