package com.example.my_app;

import android.app.Activity;
import android.os.Bundle;

// My code begin
import android.view.View;
import android.widget.EditText;
// My code end

public class my_app_activity extends Activity
{
    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.main);
    }

    // My code begin

    /** Called when the user clicks the button */
    public void onButtonClickMethod(View view)
    {
        // do smth
        EditText editText = (EditText) findViewById(R.id.edit_message);
        String message = editText.getText().toString();
        message += " + another message\n";
        editText.setText(message);
    }
    // My code end
}

