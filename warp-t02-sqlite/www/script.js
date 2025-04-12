// DOM Elements
const userList = document.getElementById('user-list');
const createForm = document.getElementById('create-form');
const editForm = document.getElementById('edit-form');
const editSection = document.getElementById('edit-section');
const notification = document.getElementById('notification');
const deleteBtn = document.getElementById('delete-btn');
const cancelBtn = document.getElementById('cancel-btn');

// API URL
const API_URL = '/api/users';

// Initially hide the edit section
editSection.style.display = 'none';

// Cancel button functionality
cancelBtn.addEventListener('click', () => {
  editSection.style.display = 'none';
  editForm.reset();
});

// Show notification
function showNotification(message, type) {
  notification.textContent = message;
  notification.className = 'notification ' + type;
  notification.style.display = 'block';
  
  // Hide notification after 3 seconds
  setTimeout(() => {
    notification.style.display = 'none';
  }, 3000);
}

// Load users from API
async function loadUsers() {
  try {
    const response = await fetch(API_URL);
    if (!response.ok) {
      throw new Error('Failed to load users');
    }
    
    const users = await response.json();
    renderUsers(users);
  } catch (error) {
    showNotification(error.message, 'error');
  }
}

// Render users in the table
function renderUsers(users) {
  userList.innerHTML = '';
  
  if (!users || users.length === 0) {
    const row = document.createElement('tr');
    row.innerHTML = '<td colspan="3">No users found</td>';
    userList.appendChild(row);
    return;
  }
  
  users.forEach(user => {
    const row = document.createElement('tr');
    row.innerHTML = `
      <td>${user.id}</td>
      <td>${user.name}</td>
      <td>
        <div class="action-buttons">
          <button onclick="editUser(${user.id})">Edit</button>
          <button class="delete" onclick="deleteUser(${user.id})">Delete</button>
        </div>
      </td>
    `;
    userList.appendChild(row);
  });
}

// Create user
createForm.addEventListener('submit', async (e) => {
  e.preventDefault();
  
  const name = document.getElementById('create-name').value.trim();
  
  if (!name) {
    showNotification('Please enter a name', 'error');
    return;
  }
  
  try {
    const response = await fetch(API_URL, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({ name })
    });
    
    if (!response.ok) {
      throw new Error('Failed to create user');
    }
    
    const newUser = await response.json();
    showNotification(`User ${newUser.name} created successfully`, 'success');
    createForm.reset();
    
    // Refresh user list
    loadUsers();
  } catch (error) {
    showNotification(error.message, 'error');
  }
});

// Edit user - load user data into form
async function editUser(id) {
  try {
    const response = await fetch(`${API_URL}/${id}`);
    if (!response.ok) {
      throw new Error('Failed to load user details');
    }
    
    const user = await response.json();
    
    document.getElementById('edit-id').value = user.id;
    document.getElementById('edit-name').value = user.name;
    
    // Show edit section
    editSection.style.display = 'block';
    document.getElementById('edit-name').focus();
  } catch (error) {
    showNotification(error.message, 'error');
  }
}

// Update user
editForm.addEventListener('submit', async (e) => {
  e.preventDefault();
  
  const id = document.getElementById('edit-id').value;
  const name = document.getElementById('edit-name').value.trim();
  
  if (!name) {
    showNotification('Please enter a name', 'error');
    return;
  }
  
  try {
    const response = await fetch(`${API_URL}/${id}`, {
      method: 'PUT',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({ name })
    });
    
    if (!response.ok) {
      throw new Error('Failed to update user');
    }
    
    const updatedUser = await response.json();
    showNotification(`User ${updatedUser.name} updated successfully`, 'success');
    
    // Refresh user list
    loadUsers();
  } catch (error) {
    showNotification(error.message, 'error');
  }
});

// Delete user from edit form
deleteBtn.addEventListener('click', async () => {
  const id = document.getElementById('edit-id').value;
  
  if (confirm('Are you sure you want to delete this user?')) {
    await deleteUser(id);
  }
});

// Delete user function
async function deleteUser(id) {
  try {
    const response = await fetch(`${API_URL}/${id}`, {
      method: 'DELETE'
    });
    
    if (!response.ok) {
      throw new Error('Failed to delete user');
    }
    
    showNotification('User deleted successfully', 'success');
    
    // Refresh user list
    loadUsers();
  } catch (error) {
    showNotification(error.message, 'error');
  }
}

// Load users when page loads
document.addEventListener('DOMContentLoaded', loadUsers);
