require("@testing-library/jest-dom/extend-expect");
const t = require("@testing-library/react");
const React = require("react");
const queryMock = require("./queryMock");

const { test_lazyFragment } = require("./Test_lazyFragment.bs");

describe("Lazy fragments", () => {
  test("lazy fragment works", async () => {
    queryMock.mockQuery({
      name: "TestLazyFragmentQuery",
      variables: {
        id: "user-1",
      },
      data: {
        node: {
          __typename: "User",
          id: "user-1",
          firstName: "First",
          lastName: "Last",
        },
      },
    });

    t.render(test_lazyFragment());
    await t.screen.findByText("Name: First Last");
  });
});
