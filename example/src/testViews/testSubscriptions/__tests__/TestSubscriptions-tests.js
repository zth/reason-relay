import * as React from 'react';
import { make as TestSubscriptions } from '../TestSubscriptions.bs';
import { act } from 'react-dom/test-utils';
import { QueryMock } from 'graphql-query-test-mock';
import {
  render,
  fireEvent,
  cleanup,
  waitForElement
} from '@testing-library/react';
import { makeEnvironmentWithSubscription } from '../../../TestUtils.bs';
import { make as EnvironmentProvider } from '../../../EnvironmentProvider.bs';

global.fetch = require('node-fetch');

let queryMock;

beforeEach(() => {
  queryMock = new QueryMock();
  queryMock.setup('http://localhost:4000');
});

afterEach(() => {
  queryMock.cleanup();
  queryMock.reset();
  cleanup();
});

describe('TestSubscriptions', () => {
  test('subscriptions', async () => {
    let r;
    let controller;

    act(() => {
      r = render(
        <EnvironmentProvider
          environment={makeEnvironmentWithSubscription(c => {
            controller = c;
          })}
        >
          <TestSubscriptions />
        </EnvironmentProvider>
      );
    });

    await waitForElement(() => r.getByText('0 books added.'));
  });
});
