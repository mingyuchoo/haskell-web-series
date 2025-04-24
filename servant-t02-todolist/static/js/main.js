/**
 * Initialize the application when the DOM is fully loaded
 */
document.addEventListener('DOMContentLoaded', () => {
  showDefaultMessage();
  loadTodos();

  const todoForm = document.getElementById('todo-form');
  todoForm?.addEventListener('submit', handleFormSubmit);
  
  // Set up interval to update relative times every minute
  setInterval(updateRelativeTimes, 60000);
});

/**
 * Update all relative time displays on the page
 */
const updateRelativeTimes = () => {
  const timeElements = document.querySelectorAll('.relative-time[data-timestamp]');
  
  timeElements.forEach(element => {
    const timestamp = element.getAttribute('data-timestamp');
    if (timestamp) {
      element.textContent = formatRelativeTime(timestamp);
    }
  });
}

/**
 * Fetch all todos from the API and display them
 * Handles Haskell's Either type responses (Right for success, Left for errors)
 * @returns {Promise<void>}
 */
const loadTodos = async () => {
  try {
    const response = await fetch('/api/todos');
    
    if (!response.ok) {
      throw new Error(`HTTP error! Status: ${response.status}`);
    }
    
    const responseText = await response.text();
    
    if (!responseText?.trim()) {
      displayTodos([]);
      return;
    }
    
    try {
      const parsedData = JSON.parse(responseText);
      
      // Extract todos from Either type (Right field) or use directly if it's an array
      const todos = Array.isArray(parsedData) ? parsedData : parsedData?.Right || [];
      
      displayTodos(todos);
    } catch (parseError) {
      console.error('JSON parse error:', parseError);
      showMessage('Error parsing todos data. Please try again.', 'error');
    }
  } catch (error) {
    console.error('Error loading todos:', error);
    showMessage('Error loading todos. Please try again.', 'error');
  }
}

/**
 * Format a timestamp as a relative time string (e.g., '2 minutes ago', '3 hours ago')
 * @param {string} timestamp - ISO timestamp string
 * @returns {string} - Formatted relative time string
 */
const formatRelativeTime = (timestamp) => {
  const date = new Date(timestamp);
  const now = new Date();
  const diffMs = now - date;
  const diffSec = Math.floor(diffMs / 1000);
  const diffMin = Math.floor(diffSec / 60);
  const diffHour = Math.floor(diffMin / 60);
  const diffDay = Math.floor(diffHour / 24);
  const diffMonth = Math.floor(diffDay / 30);
  const diffYear = Math.floor(diffMonth / 12);

  // Store the absolute date for tooltip
  const absoluteDate = date.toLocaleString();
  
  if (diffSec < 60) {
    return `${diffSec} second${diffSec !== 1 ? 's' : ''} ago`;
  } else if (diffMin < 60) {
    return `${diffMin} minute${diffMin !== 1 ? 's' : ''} ago`;
  } else if (diffHour < 24) {
    return `${diffHour} hour${diffHour !== 1 ? 's' : ''} ago`;
  } else if (diffDay < 30) {
    return `${diffDay} day${diffDay !== 1 ? 's' : ''} ago`;
  } else if (diffMonth < 12) {
    return `${diffMonth} month${diffMonth !== 1 ? 's' : ''} ago`;
  } else {
    return `${diffYear} year${diffYear !== 1 ? 's' : ''} ago`;
  }
}

/**
 * Display todos in the table
 * @param {Array} todos - Array of todo objects
 */
const displayTodos = (todos) => {
  const tableBody = document.getElementById('todos-table-body');
  if (!tableBody) return;

  tableBody.innerHTML = '';
  
  if (!todos?.length) {
    tableBody.innerHTML = '<tr><td colspan="6">No todos found</td></tr>';
    return;
  }

  const todosHtml = todos.map(({ todoId, todoTitle, createdAt, priority, isCompleted }) => {
    // Format the priority with appropriate class
    const priorityClass = `priority-${priority.toLowerCase()}`;
    const priorityDisplay = `<span class="${priorityClass}">${priority}</span>`;
    
    // Format the status with appropriate class
    const statusClass = isCompleted ? 'status-completed' : 'status-pending';
    const statusDisplay = `<span class="${statusClass}">${isCompleted ? 'Completed' : 'Pending'}</span>`;
    
    // Format the date as relative time
    const relativeTime = formatRelativeTime(createdAt);
    const absoluteDate = new Date(createdAt).toLocaleString();
    
    return `
    <tr>
      <td>${todoId}</td>
      <td>${todoTitle}</td>
      <td class="relative-time" data-timestamp="${createdAt}" title="${absoluteDate}">${relativeTime}</td>
      <td>${priorityDisplay}</td>
      <td>${statusDisplay}</td>
      <td>
        <button class="btn" onclick="editTodo(${todoId}, '${todoTitle}', '${priority}', ${isCompleted})">Edit</button>
        <button class="btn btn-danger" onclick="deleteTodo(${todoId})">Delete</button>
      </td>
    </tr>
  `}).join('');
  
  tableBody.innerHTML = todosHtml;
}

/**
 * Handle form submission for creating or updating a todo
 * @param {Event} event - The form submission event
 * @returns {Promise<void>}
 */
const handleFormSubmit = async (event) => {
  event.preventDefault();
  
  const todoId = document.getElementById('todoId').value;
  const todoTitle = document.getElementById('todoTitle').value;
  const todoPriority = document.getElementById('todoPriority').value;
  const todoCompleted = document.getElementById('todoCompleted').checked;
  const formMode = document.getElementById('form-mode').value;
  
  if (!todoTitle?.trim()) {
    showMessage('Please enter a todo title', 'error');
    return;
  }
  
  try {
    const isCreate = formMode === 'create';
    const endpoint = isCreate ? '/api/todos' : `/api/todos/${todoId}`;
    const method = isCreate ? 'POST' : 'PUT';
    const payload = isCreate 
      ? { newTodoName: todoTitle, newTodoPriority: todoPriority }
      : { todoId: parseInt(todoId, 10), todoTitle, priority: todoPriority, isCompleted: todoCompleted };
    
    const response = await fetch(endpoint, {
      method,
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(payload)
    });
    
    if (!response.ok) {
      throw new Error(`HTTP error! Status: ${response.status}`);
    }
    
    const data = await response.json();
    
    // Handle Haskell's Either type response (Left for errors)
    if (data.Left) {
      showMessage(`Validation error: ${data.Left.errorMessage || 'Unknown error'}`, 'error');
      return;
    }
    
    // Handle other error formats
    if (data?.errorMessage) {
      showMessage(`Validation error: ${data.errorMessage}`, 'error');
      return;
    }
    
    resetForm();
    loadTodos();
    
    const successMessage = isCreate ? 'Todo created successfully!' : 'Todo updated successfully!';
    showMessage(successMessage, 'success');
  } catch (error) {
    console.error('Error:', error);
    showMessage(`Error: ${error.message}`, 'error');
  }
}

/**
 * Set up form for creating a new todo
 */
const setupCreateForm = () => {
  document.getElementById('form-title').textContent = 'Create New Todo';
  document.getElementById('form-mode').value = 'create';
  document.getElementById('todoId').value = '';
  document.getElementById('todoTitle').value = '';
  document.getElementById('todoPriority').value = 'Medium';
  document.getElementById('todoCompleted').checked = false;
  document.getElementById('completed-group').style.display = 'none';
  document.getElementById('submit-btn').textContent = 'Create Todo';
}

/**
 * Set up form for editing an existing todo
 * @param {number} todoId - ID of the todo to edit
 * @param {string} todoTitle - Title of the todo to edit
 */
const editTodo = (todoId, todoTitle, priority, isCompleted) => {
  document.getElementById('form-title').textContent = 'Edit Todo';
  document.getElementById('form-mode').value = 'update';
  document.getElementById('todoId').setAttribute('readonly', 'readonly');
  document.getElementById('todoId').value = todoId;
  document.getElementById('todoTitle').value = todoTitle;
  document.getElementById('todoPriority').value = priority;
  document.getElementById('todoCompleted').checked = isCompleted;
  document.getElementById('completed-group').style.display = 'block';
  document.getElementById('submit-btn').textContent = 'Update Todo';
  
  // Scroll to form
  document.getElementById('todo-form').scrollIntoView({ behavior: 'smooth' });
}

/**
 * Delete a todo by ID
 * @param {number} todoId - ID of the todo to delete
 * @returns {Promise<void>}
 */
const deleteTodo = async (todoId) => {
  if (!confirm(`Are you sure you want to delete todo with ID ${todoId}?`)) {
    return;
  }
  
  try {
    const response = await fetch(`/api/todos/${todoId}`, {
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

/**
 * Reset the form to its initial state
 */
const resetForm = () => {
  setupCreateForm();
  showDefaultMessage();
}

/**
 * Show a message to the user
 * @param {string} message - Message to display
 * @param {string} type - Message type ('error' or 'success')
 */
const showMessage = (message, type) => {
  const messageContainer = document.getElementById('message-container');
  if (!messageContainer) return;
  
  const alertClass = type === 'error' ? 'alert-danger' : 'alert-success';
  
  messageContainer.innerHTML = `
    <div class="alert ${alertClass}">
      ${message}
    </div>
  `;
  
  // Clear the message after 5 seconds and show default message
  setTimeout(showDefaultMessage, 5000);
}

/**
 * Show the default message
 */
const showDefaultMessage = () => {
  const messageContainer = document.getElementById('message-container');
  if (!messageContainer) return;
  
  messageContainer.innerHTML = `
    <div class="alert alert-info">
      Add your new Todo or search for an existing Todo
    </div>
  `;
}
