document.addEventListener('DOMContentLoaded', function() {
  // Load all todos when the page loads
  loadTodos();

  // Set up event listeners
  const todoForm = document.getElementById('todo-form');
  if (todoForm) {
    todoForm.addEventListener('submit', handleFormSubmit);
  }
});

// Function to load all todos from the API
async function loadTodos() {
  try {
    const response = await fetch('/todos');
    const todos = await response.json();
    displayTodos(todos);
  } catch (error) {
    console.error('Error loading todos:', error);
    showMessage('Error loading todos. Please try again.', 'error');
  }
}

// Function to display todos in the table
function displayTodos(todos) {
  const tableBody = document.getElementById('todos-table-body');
  if (!tableBody) return;

  tableBody.innerHTML = '';
  
  if (todos.length === 0) {
    const row = document.createElement('tr');
    row.innerHTML = '<td colspan="4">No todos found</td>';
    tableBody.appendChild(row);
    return;
  }

  todos.forEach(todo => {
    const row = document.createElement('tr');
    row.innerHTML = `
      <td>${todo.todoId}</td>
      <td>${todo.todoTitle}</td>
      <td>
        <button class="btn" onclick="editTodo(${todo.todoId}, '${todo.todoTitle}')">Edit</button>
        <button class="btn btn-danger" onclick="deleteTodo(${todo.todoId})">Delete</button>
      </td>
    `;
    tableBody.appendChild(row);
  });
}

// Function to handle form submission (create or update todo)
async function handleFormSubmit(event) {
  event.preventDefault();
  
  const todoId = document.getElementById('todoId').value;
  const todoTitle = document.getElementById('todoTitle').value;
  
  if (!todoTitle) {
    showMessage('Please enter a todo name', 'error');
    return;
  }
  
  const formMode = document.getElementById('form-mode').value;
  
  try {
    let response;
    
    if (formMode === 'create') {
      // For creation, we only need todoTitle as todoId is auto-generated
      const newTodoData = {
        newTodoName: todoTitle
      };
      
      response = await fetch('/todos', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json'
        },
        body: JSON.stringify(newTodoData)
      });
    } else if (formMode === 'update') {
      // For updates, we need both todoId and todoTitle
      const todoData = {
        todoId: parseInt(todoId),
        todoTitle: todoTitle
      };
      
      response = await fetch(`/todos/${todoId}`, {
        method: 'PUT',
        headers: {
          'Content-Type': 'application/json'
        },
        body: JSON.stringify(todoData)
      });
    }
    
    if (!response.ok) {
      throw new Error(`HTTP error! Status: ${response.status}`);
    }
    
    // Parse the response
    const data = await response.json();
    
    // Check if there's a validation error
    if (data && data.errorMessage) {
      showMessage(`Validation error: ${data.errorMessage}`, 'error');
      return;
    }
    
    // Reset form
    resetForm();
    
    // Reload todos
    loadTodos();
    
    showMessage(formMode === 'create' ? 'Todo created successfully!' : 'Todo updated successfully!', 'success');
  } catch (error) {
    console.error('Error:', error);
    showMessage(`Error: ${error.message}`, 'error');
  }
}

// Function to set up form for creating a new todo
function setupCreateForm() {
  document.getElementById('form-title').textContent = 'Create New Todo';
  document.getElementById('form-mode').value = 'create';
  document.getElementById('todoId').value = '';
  document.getElementById('todoTitle').value = '';
  document.getElementById('submit-btn').textContent = 'Create Todo';
}

// Function to set up form for editing a todo
function editTodo(todoId, todoTitle) {
  document.getElementById('form-title').textContent = 'Edit Todo';
  document.getElementById('form-mode').value = 'update';
  document.getElementById('todoId').setAttribute('readonly', 'readonly');
  document.getElementById('todoId').value = todoId;
  document.getElementById('todoTitle').value = todoTitle;
  document.getElementById('submit-btn').textContent = 'Update Todo';
  
  // Scroll to form
  document.getElementById('todo-form').scrollIntoView({ behavior: 'smooth' });
}

// Function to delete a todo
async function deleteTodo(todoId) {
  if (!confirm(`Are you sure you want to delete todo with ID ${todoId}?`)) {
    return;
  }
  
  try {
    const response = await fetch(`/todos/${todoId}`, {
      method: 'DELETE'
    });
    
    if (!response.ok) {
      throw new Error(`HTTP error! Status: ${response.status}`);
    }
    
    loadTodos();
    showMessage('Todo deleted successfully!', 'success');
  } catch (error) {
    console.error('Error deleting todo:', error);
    showMessage(`Error deleting todo: ${error.message}`, 'error');
  }
}

// Function to reset the form
function resetForm() {
  setupCreateForm();
}

// Function to show a message to the todo
function showMessage(message, type) {
  const messageContainer = document.getElementById('message-container');
  if (!messageContainer) return;
  
  const alertClass = type === 'error' ? 'alert-danger' : 'alert-success';
  
  messageContainer.innerHTML = `
    <div class="alert ${alertClass}">
      ${message}
    </div>
  `;
  
  // Clear the message after 5 seconds
  setTimeout(() => {
    messageContainer.innerHTML = '';
  }, 5000);
}
