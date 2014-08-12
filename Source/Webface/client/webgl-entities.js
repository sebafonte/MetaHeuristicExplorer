// Global default vars
var sampleEntity = "cos(x) * cos(y)";
var sampleEntityType = "entity-bw";


// Draw functions
function drawEntity(entity, entityType, canvas) {
	// get context
	var gl = canvas.getContext("experimental-webgl");

	// switch entity type
	switch(entityType)
	{
		case "entity-bw":
			drawEntityImageBW(canvas, gl, entity);	
	}
}

function drawEntityImageBW() {
	// Setup a GLSL program
	var vertexShader = createShaderFromScriptElement(gl, "shader-vs");
	var fragmentShader = createShaderFromScriptElement(gl, "shader-fs");
	var program = createProgram(gl, [vertexShader, fragmentShader]);
	gl.useProgram(program);

	// Look up where the vertex data needs to go
	var positionLocation = gl.getAttribLocation(program, "a_position");

	// Create a buffer and put a single clipspace rectangle in it (2 triangles)
	var buffer = gl.createBuffer();
	gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
	gl.bufferData(
		gl.ARRAY_BUFFER, 
		new Float32Array([
			-1.0, -1.0, 
			 1.0, -1.0, 
			-1.0,  1.0, 
			-1.0,  1.0, 
			 1.0, -1.0, 
			 1.0,  1.0]), 
		gl.STATIC_DRAW);
	gl.enableVertexAttribArray(positionLocation);
	gl.vertexAttribPointer(positionLocation, 2, gl.FLOAT, false, 0, 0);

	// draw
	gl.drawArrays(gl.TRIANGLES, 0, 6); 	
}
 
function drawShaderEntity () { 
	// setup a GLSL program
	var vertexShader = createShaderFromScriptElement(gl, "2d-vertex-shader");
	var fragmentShader = createShaderFromScriptElement(gl, "2d-fragment-shader");
	var program = createProgram(gl, [vertexShader, fragmentShader]);
	gl.useProgram(program);

	// look up where the vertex data needs to go
	var positionLocation = gl.getAttribLocation(program, "a_position");

	// Create a buffer and put a single clipspace rectangle in it (2 triangles)
	var buffer = gl.createBuffer();
	gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
	gl.bufferData(
		gl.ARRAY_BUFFER, 
		new Float32Array([
			-1.0, -1.0, 
			 1.0, -1.0, 
			-1.0,  1.0, 
			-1.0,  1.0, 
			 1.0, -1.0, 
			 1.0,  1.0]), 
		gl.STATIC_DRAW);
	gl.enableVertexAttribArray(positionLocation);
	gl.vertexAttribPointer(positionLocation, 2, gl.FLOAT, false, 0, 0);

	// draw
	gl.drawArrays(gl.TRIANGLES, 0, 6); 
}

function drawEntityImageVecColor() {
	
}
 
function drawTextureEntity () { 
	
}

