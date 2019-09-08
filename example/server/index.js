const { ApolloServer, gql } = require('apollo-server');
const { toGlobalId, connectionFromArray } = require('graphql-relay');

let books = [
  {
    type: 'Book',
    id: toGlobalId('Book', '1'),
    title: 'Harry Potter and the Chamber of Secrets',
    author: 'J.K. Rowling',
    shelfId: toGlobalId('Shelf', '1'),
    status: 'Discontinued'
  },
  {
    type: 'Book',
    id: toGlobalId('Book', '2'),
    title: 'Jurassic Park',
    author: 'Michael Crichton',
    shelfId: toGlobalId('Shelf', '2'),
    status: 'Published'
  }
];

let bookCollection = [
  {
    type: 'BookCollection',
    id: toGlobalId('BookCollection', '1'),
    books: [books[1], books[2]]
  }
];

let shelves = [
  {
    id: toGlobalId('Shelf', '1'),
    name: 'Shelf 1',
    contents: [books[0], bookCollection[0]]
  },
  {
    id: toGlobalId('Shelf', '2'),
    name: 'Shelf 2',
    contents: []
  }
];

const typeDefs = gql`
  type Shelf {
    id: ID!
    name: String!
    contents: [ShelfContent!]!
  }

  enum BookStatus {
    Draft
    Published
    Discontinued
  }

  type Book {
    id: ID!
    title: String!
    author: String!
    shelf: Shelf!
    status: BookStatus
  }

  type BookCollection {
    id: ID!
    books: [Book!]!
  }

  type PageInfo {
    hasNextPage: Boolean!
    hasPreviousPage: Boolean!
    endCursor: String
    startCursor: String
  }

  type BookEdge {
    cursor: String!
    node: Book!
  }

  type BookConnection {
    edges: [BookEdge]!
    pageInfo: PageInfo!
  }

  union ShelfContent = Book | BookCollection

  type Query {
    book(id: ID!): Book
    books(status: BookStatus): [Book!]!
    booksPaginated(first: Int, after: String): BookConnection
    fromShelf(shelfId: ID!): [ShelfContent!]
  }

  input AddBookInput {
    clientMutationId: String
    title: String!
    author: String!
  }

  type AddBookPayload {
    clientMutationId: String
    book: Book
  }

  input DeleteBookInput {
    clientMutationId: String
    id: ID!
  }

  type DeleteBookPayload {
    clientMutationId: String
    deleted: Boolean!
  }

  input UpdateBookInput {
    clientMutationId: String
    id: ID!
    title: String!
    author: String!
    status: BookStatus
  }

  type UpdateBookPayload {
    clientMutationId: String
    book: Book
  }

  type Mutation {
    addBook(input: AddBookInput!): AddBookPayload!
    deleteBook(input: DeleteBookInput!): DeleteBookPayload!
    updateBook(input: UpdateBookInput!): UpdateBookPayload!
  }
  
  type Subscription {
    bookAdded: Book!
  }
`;

const resolvers = {
  Query: {
    book: (_, args) => books.find(b => b.id === args.id),
    books: (_, args) =>
      books.filter(b => (args.status ? b.status === args.status : true)),
    booksPaginated: (_, args) => connectionFromArray(books, args),
    fromShelf: (_, args) => {
      let shelf = shelves.find(s => s.id === args.shelfId);
      return shelf ? shelf.contents : null;
    }
  },
  ShelfContent: {
    __resolveType(obj) {
      return obj.type || null;
    }
  },
  Book: {
    shelf: book => shelves.find(s => s.id === book.shelfId)
  },
  Mutation: {
    addBook: (_, args) => {
      const book = {
        id: toGlobalId('Book', Math.random()),
        title: args.input.title,
        author: args.input.author,
        shelfId: shelves[0].id
      };

      books.push(book);

      return { book };
    },
    deleteBook: (_, args) => {
      const bookIndex = books.findIndex(b => b.id === args.id);

      if (bookIndex) {
        books.splice(bookIndex, 1);
      }

      return { deleted: bookIndex > -1 };
    },
    updateBook: (_, args) => {
      const book = books.find(b => b.id === args.input.id);

      if (book) {
        book.title = args.input.title;
        book.author = args.input.author;
        book.status = args.input.status;
      }

      return { book };
    }
  }
};

const server = new ApolloServer({
  typeDefs,
  resolvers
});

server.listen().then(({ url }) => {
  console.log(`🚀  Server ready at ${url}`);
});
