/**
 * Right now, we need to post-process the generated JS some. We do the following here:
 * - Alter all require()-operations to point at our generated files. This is because
 *   Relay by default outputs require calls for @refetchable operations which point to
 *   where the equivalent JS module would be located, and that we're not currently allowed
 *   to alter the generated module names and import paths. I've considered PRing support for
 *   altering the paths "for real" through the language plugin, but this actually seems to
 *   be working fine.
 */

export function processConcreteText(concreteText: string): string {
  let requireRegexp = /(require\('.\/)([A-Za-z_.0-9/]+)(.graphql.\w*'\))/gm;
  let str = concreteText;

  let result;

  while ((result = requireRegexp.exec(concreteText)) !== null) {
    let [fullStr, _, moduleName] = result;
    str = str.replace(fullStr, `require('./${moduleName}_graphql.bs.js')`);
  }

  return str;
}
