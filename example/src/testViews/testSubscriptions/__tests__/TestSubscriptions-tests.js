import * as React from 'react';
import { make as TestSubscriptions } from '../TestSubscriptions.bs';
import { act } from 'react-dom/test-utils';
import { render, cleanup, waitForElement } from '@testing-library/react';
import { makeEnvironmentWithSubscription } from '../../../TestUtils.bs';
import { make as EnvironmentProvider } from '../../../EnvironmentProvider.bs';

global.fetch = require('node-fetch');

afterEach(() => {
  cleanup();
});

describe('TestSubscriptions', () => {
  test('subscriptions', async () => {
    let r;
    let [environment, controller] = makeEnvironmentWithSubscription();

    act(() => {
      r = render(
        <EnvironmentProvider environment={environment}>
          <TestSubscriptions />
        </EnvironmentProvider>
      );
    });

    await waitForElement(() => r.getByText('0 books added.'));

    act(() => {
      controller[0][0]({
        data: {
          bookAdded: {
            id: 'book-1',
            title: 'Some book',
            author: 'Some author'
          }
        }
      });
    });

    await waitForElement(() => r.getByText('1 books added.'));
  });
});
